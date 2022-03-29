import React, {
  useCallback, useEffect, useMemo,
} from 'react';
import {
  DataSet, Form, TextField,
} from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import { isEqual } from 'lodash';
import { fieldApi, quickFilterApi } from '@/api';
import { IModalProps } from '@/common/types';
import useIsInProgram from '@/hooks/useIsInProgram';
import {
  getAttributeRelation, getFastSearchAttribute, processWaitSubmitData, transformSearchConditionListToEditData,
} from './utils';
import FastSearchForm from './FastSearchForm';
import { IFastSearchEditData } from './types';
import styles from './index.less';
import useIsWaterfall from '@/hooks/useIsWaterfall';

interface FastSearchProps {
  modal?: IModalProps,
  isInProgram?: boolean
  data?: IFastSearchEditData
  onOK?: () => void
}

const FastSearch: React.FC<FastSearchProps> = ({
  modal, data: originData, isInProgram, onOK,
}) => {
  const isEditMode = !!originData;
  const { isWaterfallAgile } = useIsWaterfall();
  const handleCheckName = useCallback(async (value: any) => {
    if (originData?.name === value) {
      return true;
    }
    return quickFilterApi.checkName(value.trim()).then((res: boolean) => (res ? '快速搜索名称重复' : true));
  }, [originData?.name]);
  const ds = useMemo(() => new DataSet({
    autoCreate: false,
    autoQuery: false,
    fields: [
      {
        name: 'name',
        type: 'string' as any,
        label: '名称',
        maxLength: 10,
        required: true,
        trim: 'both' as any,
        validator: (value) => handleCheckName(value),
      },
      {
        name: 'description', type: 'string' as any, label: '描述', maxLength: 30,
      },
    ],
  }), [handleCheckName]);

  const searchConditionAttributeDs = useMemo(() => new DataSet({
    autoCreate: false,
    autoQuery: false,
    paging: false,
  }), []);
  const loadAttributeData = useCallback(async () => {
    const data = await Promise.all<[any[], any[]]>([quickFilterApi.loadField(), fieldApi.getCustomFields(isWaterfallAgile ? '' : undefined)])
      .then(([preDefinedField, customField]) => ([...preDefinedField, ...isWaterfallAgile ? [{
        fieldCode: 'progress', fieldType: 'number', type: 'long', name: '进度',
      }] : [], ...isInProgram ? [{ fieldCode: 'feature', type: 'long', name: '特性' }] : [], ...customField].map(getFastSearchAttribute) || []));
    return data;
  }, [isInProgram, isWaterfallAgile]);
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
      { name: 'name', bind: 'attribute.name' },
      { name: 'fieldCode', type: 'string' as any, bind: 'attribute.fieldCode' },
      { name: 'id', bind: 'attribute.id' },
      // { name: 'isCustomField', bind: 'attribute.id' },
      { name: 'fieldType', type: 'string' as any, bind: 'attribute.fieldType' },
      { name: 'fieldOptions', bind: 'attribute.fieldOptions', ignore: 'always' as any },
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
        dynamicProps: {
          required: ({ record }) => !!record.index,
        },
      },
      {
        name: 'value',
        label: '值',
        required: true,
        dynamicProps: {
          disabled: ({ record }) => !record.get('relation'),
          multiple: ({ record }) => !['text', 'input'].includes(record.get('fieldType')) && ['include', 'exclude'].includes(record.get('relation')),
        },
      },
      { name: 'valueBindValue', label: '值', bind: 'value.value' },
      { name: 'valueText', type: 'string' as any, bind: 'value.meaning' },
    ],
    events: {
      update: ({
        record, name, value, oldValue,
      }: any) => {
        record.init('_editData', false);
        if (name === 'attribute' && !isEqual(value?.fieldCode, oldValue?.fieldCode)) {
          record.init('relation', undefined);
          record.init('value', undefined);
        } else if (name === 'relation' && !isEqual(value, oldValue)) {
          record.init('value', undefined);
        }
      },
    },
  }), [searchConditionAttributeDs]);
  useEffect(() => {
    // 初始化名称 描述 主体的dataSet
    ds.create(originData);
    !isEditMode && searchConditionDs.create();
    loadAttributeData().then((data) => {
      searchConditionAttributeDs.loadData(data);// 属性列加载数据
      // 有编辑数据则初始化 筛选条件
      isEditMode && searchConditionDs.loadData(transformSearchConditionListToEditData(originData?.searchConditionList!, data));
    });
  }, [ds, isEditMode, loadAttributeData, originData, searchConditionAttributeDs, searchConditionDs]);
  const handleSubmit = useCallback(async () => {
    if (!await ds.validate() || !await searchConditionDs.validate()) {
      return false;
    }
    const submitData = processWaitSubmitData(ds, searchConditionDs);
    try {
      originData ? await quickFilterApi.update(originData.filterId, { ...submitData, objectVersionNumber: originData.objectVersionNumber }) : await quickFilterApi.create(submitData);
    } catch (requestRes) {
      if (requestRes?.failed && String(requestRes?.exception || '').indexOf('Data too long') !== -1) {
        Choerodon.prompt('该筛选器筛选条件超出最大限度，请您删减筛选属性或值。', 'error');
        return false;
      } if (requestRes?.failed) {
        Choerodon.prompt(requestRes?.message, 'error');
      }
    }

    onOK && onOK();
    return true;
  }, [ds, onOK, originData, searchConditionDs]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <Form dataSet={ds} className={styles.form}>
      <TextField name="name" maxLength={10} valueChangeAction={'input' as any} />
      <FastSearchForm dataSet={searchConditionDs} />
      <TextField name="description" maxLength={30} valueChangeAction={'input' as any} />
    </Form>
  );
};
const FastSearchHoc: React.FC<FastSearchProps> = (props) => {
  const { isInProgram, loading } = useIsInProgram();
  return !loading ? <FastSearch {...props} isInProgram={isInProgram} /> : null;
};
export default FastSearchHoc;
