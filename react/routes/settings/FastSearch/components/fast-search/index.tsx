import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import {
  DataSet, Form, TextField,
} from 'choerodon-ui/pro';
import { pick, merge } from 'lodash';
import { fieldApi, quickFilterApi } from '@/api';
import { IModalProps } from '@/common/types';
import {
  getAttributeRelation, getFastSearchAttribute, transformRelationValueToOperation, getCustomFieldType, processWaitSubmitData, transformSearchConditionListToEditData,
} from './utils';
import FastSearchForm from './FastSearchForm';
import { IFastSearchEditData } from './types';
import styles from './index.less';

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
      { name: 'name', bind: 'attribute.name' },
      { name: 'fieldCode', type: 'string' as any, bind: 'attribute.fieldCode' },
      { name: 'isCustomField', type: 'boolean' as any, bind: 'attribute.id' },
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
        // ignore: 'always' as any, // 忽略提交 不需要的字段
      },
      { name: 'valueBindValue', label: '值', bind: 'value.value' },
      { name: 'valueText', type: 'string' as any, bind: 'value.meaning' },
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
    console.log('handleSubmit', submitData, searchConditionDs.toJSONData());
    // return false;
    originData ? quickFilterApi.update(originData.filterId, { ...submitData, objectVersionNumber: originData.objectVersionNumber }) : quickFilterApi.create(submitData);
    // quickFilterApi.update()
    onOK && onOK();
    return true;
  }, [ds, onOK, originData, searchConditionDs]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <Form dataSet={ds} className={styles.form}>
      <TextField name="name" maxLength={10} />
      <FastSearchForm dataSet={searchConditionDs} />
      <TextField name="description" maxLength={30} />
    </Form>
  );
};

export default FastSearch;
