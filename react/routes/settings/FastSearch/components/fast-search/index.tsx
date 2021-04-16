import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import {
  DataSet, Form, TextField,
} from 'choerodon-ui/pro';
import { pick, merge } from 'lodash';
import { fieldApi, quickFilterApi } from '@/api';
import './index.less';
import { IModalProps } from '@/common/types';
import { getProjectId } from '@/utils/common';
import {
  getAttributeRelation, getFastSearchAttribute, transformRelationValueToOperation, getCustomFieldType,
} from './utils';
import FastSearchForm from './FastSearchForm';
import { IFastSearchCondition, IFastSearchEditData } from './types';

interface FastSearchProps {
  modal?: IModalProps,
  prefixCls?: string,
  isInProgram?: boolean
  data?: IFastSearchEditData
}

const FastSearch: React.FC<FastSearchProps> = ({
  modal, prefixCls = 'c7n-agile-fast-search-modal', data: originData, isInProgram,
}) => {
  const [isEditMode] = useState(() => !!originData);
  const ds = useMemo(() => new DataSet({
    autoCreate: false,
    autoQuery: false,
    fields: [
      {
        name: 'name', type: 'string' as any, label: '名称', maxLength: 10, required: true, trim: 'both' as any,
      },
      {
        name: 'description', type: 'string' as any, label: '描述', maxLength: 30,
      },
    ],
  }), []);

  const searchConditionBothRelationDs = useMemo(() => new DataSet({
    autoCreate: false,
    autoQuery: false,
    paging: false,
    data: [{ name: '且', value: 'and' }, { name: '或', value: 'or' }],
  }), []);
  const searchConditionAttributeDs = useMemo(() => new DataSet({
    autoCreate: false,
    autoQuery: false,
    paging: false,
  }), []);
  const loadAttributeData = useCallback(async () => {
    const data = await Promise.all<any[], any[]>([quickFilterApi.loadField(), fieldApi.getCustomFields()])
      .then(([preDefinedField, customField]) => ([...preDefinedField, ...isInProgram ? [{ fieldCode: 'feature', type: 'long', name: '特性' }] : [], ...customField].map(getFastSearchAttribute) || []));
    return data;
  }, [isInProgram]);
  const searchConditionDs = useMemo(() => new DataSet({
    autoCreate: false,
    autoQuery: false,
    dataToJSON: 'normal' as any,
    fields: [
      {
        name: 'attribute',
        label: '属性',
        type: 'object' as any,
        required: true,
        textField: 'name',
        valueField: 'fieldCode',
        options: searchConditionAttributeDs,
        ignore: 'always' as any, // 忽略提交 不需要的字段
      },
      {
        name: 'name',
        bind: 'attribute.name',
      },
      {
        name: 'fieldCode',
        type: 'string' as any,
        bind: 'attribute.fieldCode',
      },
      {
        name: 'isCustomField',
        type: 'boolean' as any,
        bind: 'attribute.id',
      },
      {
        name: 'fieldType',
        type: 'string' as any,
        bind: 'attribute.type',
      },
      {
        name: 'fieldOptions',
        bind: 'attribute.fieldOptions',
        ignore: 'always' as any,
      },
      {
        name: 'relation',
        label: '关系',
        textField: 'name',
        valueField: 'value',
        required: true,
        dynamicProps: {
          options: ({ record }) => {
            if (!record.get('fieldCode')) {
              return undefined;
            }
            return new DataSet({
              autoCreate: false,
              autoQuery: false,
              paging: false,
              data: getAttributeRelation(record.get('fieldCode'), record.get('fieldType')),
            });
          },
        },
      },
      {
        name: 'bothRelation',
        label: '关系',
        textField: 'name',
        valueField: 'value',
        dynamicProps: {
          required: ({ record }) => !!record.index,
        },
        options: searchConditionBothRelationDs,
      },
      {
        name: 'value',
        label: '值',
        required: true,
        dynamicProps: {
          disabled: ({ record }) => !record.get('relation'),
          multiple: ({ record }) => !['text', 'input'].includes(record.get('fieldType')) && ['include', 'exclude'].includes(record.get('relation')),
        },
        // ignore: 'always' as any, // 忽略提交 不需要的字段
      },
      {
        name: 'valueBindValue',
        label: '值',
        bind: 'value.value',
      },
      {
        name: 'valueText',
        // type: 'string' as any,
        bind: 'value.meaning',
      },
    ],
    events: {
      update: ({ record, name, value }: any) => {
        if (name === 'attribute') {
          record.init('relation', undefined);
          record.init('value', undefined);
        } else if (name === 'relation') {
          record.init('value', undefined);
        }
      },
    },
  }), [searchConditionAttributeDs, searchConditionBothRelationDs]);
  useEffect(() => {
    // 初始化名称 描述 主体的dataSet
    ds.create(originData);
    !isEditMode && searchConditionDs.create();
    loadAttributeData().then((data) => {
      searchConditionAttributeDs.loadData(data);
      // 有编辑数据则初始化 筛选条件
      isEditMode && searchConditionDs.loadData(originData?.searchConditionList.map((item) => {
        const attribute = data.find((i) => i.fieldCode === item.fieldCode);
        let { value } = item;
        // 含有选项的自定义字段处理 'null' 值 处理
        if (['is', 'notIs'].includes(item.relation)) {
          value = { value: "'null'", meaning: '空' };
        } else if (item.isCustomField && attribute?.fieldOptions) {
          const valueArr = Array.isArray(value) ? value : [value].filter(Boolean);
          const valueOptions = attribute.fieldOptions.filter((i: any) => valueArr.includes(i.id))
            .map((i: any) => ({ ...i, meaning: i.value, value: i.id }));
          value = typeof (value) === 'string' ? valueOptions[0] || value : valueOptions;
        }
        return ({ ...item, attribute, value });
      }).filter((item) => item.attribute));
    });
  }, [ds, isEditMode, loadAttributeData, originData, searchConditionAttributeDs, searchConditionDs]);
  const handleSubmit = useCallback(async () => {
    if (!await ds.validate() || !await searchConditionDs.validate()) {
      return false;
    }
    const bothRelationArr: string[] = []; //  两个相邻筛选条件关系
    const expressQueryArr: any[] = [];
    const searchConditionArr = searchConditionDs.toJSONData().map((condition: IFastSearchCondition) => {
      const value = condition.valueBindValue || condition.value;
      const operation = transformRelationValueToOperation(condition.relation);
      if (condition.bothRelation) {
        bothRelationArr.push(condition.bothRelation);
        expressQueryArr.push(condition.bothRelation.toUpperCase());
      }
      // 属性名
      expressQueryArr.push(condition.name);
      // 关系
      expressQueryArr.push(operation);
      // 显示的值
      expressQueryArr.push(Array.isArray(condition.valueText) ? `[${condition.valueText.join(',')}]` : condition.valueText || value);
      return {
        fieldCode: condition.fieldCode,
        operation,
        value: Array.isArray(value) ? `(${value.join(',')})` : `${value}`,
        predefined: !condition.isCustomField,
        customFieldType: condition.isCustomField ? getCustomFieldType(condition.fieldType) : undefined,
      };
    });

    const filterJson = JSON.stringify({
      arr: searchConditionArr,
      o: bothRelationArr,
    });
    let submitData: any = pick(ds.current?.toData(), ['name', 'description']);
    submitData = merge(submitData, {
      childIncluded: true,
      description: `${submitData.description || ''}+++${filterJson}`,
      expressQuery: expressQueryArr.join(' '),
      projectId: getProjectId(),
      quickFilterValueVOList: searchConditionArr,
      relationOperations: bothRelationArr,
    });
    console.log('handleSubmit', submitData, searchConditionDs.toJSONData());
    !originData && quickFilterApi.create(submitData).then(() => {
      modal?.close();
    });
    // quickFilterApi.update()
    return false;
  }, [ds, modal, originData, searchConditionDs]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <Form dataSet={ds} className={`${prefixCls}-form`}>
      <TextField name="name" maxLength={10} />
      <FastSearchForm dataSet={searchConditionDs} />
      <TextField name="description" maxLength={30} />
    </Form>
  );
};

export default FastSearch;
