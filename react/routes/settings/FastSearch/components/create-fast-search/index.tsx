import React, { memo, useEffect, useMemo } from 'react';
import {
  Table, DataSet, Form, Select, TextField, Modal, Button, Col, Row,
} from 'choerodon-ui/pro';
import TextArea from '@/components/TextArea';
import { fieldApi, quickFilterApi } from '@/api';
import { Observer, observer } from 'mobx-react-lite';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import './index.less';
import SelectUser from '@/components/select/select-user';
import { IModalProps } from '@/common/types';
import useSelect from '@/hooks/useSelect';
import SelectFastSearchAttribute from './SelectFastSearchAttribute';
import { getAttributeRelation } from './utils';

function FastSearchFormItemOrigin({ record, onDelete }: { record: Record, onDelete: (record: Record) => void }) {
  const prefixCls = 'c7n-agile-fast-search-form-item';

  return (
    <div className={prefixCls}>
      <Form record={record} columns={5}>
        {record.index ? <Select name="bothRelation" colSpan={1} style={{ width: 100 }} /> : null}
        <Select name="attribute" colSpan={2} searchable searchMatcher={({ record: attributeRecord, text }) => String(attributeRecord.get('name')).toLocaleLowerCase().indexOf(text.toLocaleLowerCase()) !== -1} />
        <Select name="relation" colSpan={1} style={{ width: 100 }} />
        {/* @ts-ignore */}
        <SelectUser name="value" colSpan={2} />
      </Form>
      <Button icon="delete" onClick={() => onDelete(record)} disabled={!record.index} className={`${prefixCls}-del-btn`} />
    </div>
  );
}
const FastSearchFormItem = memo(FastSearchFormItemOrigin, (prevProps, nextProps) => {
  if (prevProps === nextProps) {
    return false;
  }
  return true;
});
function FastSearchForm({ dataSet }: { dataSet: DataSet }) {
  return (
    <Observer>
      {() => (
        <div style={{ width: '100%' }}>
          {dataSet.map((record) => <FastSearchFormItem record={record} onDelete={(r) => dataSet.delete(r, false)} key={`FastSearchFormItem-${record.id}`} />)}
          <Button icon="add" onClick={() => dataSet.create()} color={'primary' as any} style={{ width: 'max-content' }}>添加筛选</Button>
        </div>
      )}
    </Observer>
  );
}
const CreateFastSearch: React.FC<{ modal?: IModalProps, isInProgram?: boolean }> = ({ modal, isInProgram }) => {
  const prefixCls = 'c7n-agile-fast-search-form';
  const ds = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'name', type: 'string' as any, label: '名称', maxLength: 10, required: true,
      },
      {
        name: 'description', type: 'string' as any, label: '描述', maxLength: 30,
      },
    ],
  }), []);
  async function loadAttributeData() {
    const data = await Promise.all<any[], any[]>([quickFilterApi.loadField(), fieldApi.getCustomFields()])
      .then(([preDefinedField, customField]) => ([...preDefinedField, ...isInProgram ? [{ fieldCode: 'feature', type: 'long', name: '特性' }] : [], ...customField].map((field) => ({ ...field, fieldCode: field.code || field.fieldCode, type: field.fieldType || field.type })) || []));
    return data;
  }
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

  useEffect(() => {
    loadAttributeData().then((data) => {
      searchConditionAttributeDs.loadData(data);
    });
  }, []);

  const searchConditionDs = useMemo(() => new DataSet({
    autoCreate: true,
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
            console.log('record', record.get('attribute'), record.toData());
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
      },
    ],
  }), [searchConditionAttributeDs, searchConditionBothRelationDs]);
  const handleSubmit = async () => {
    if (!await ds.validate() || !await searchConditionDs.validate()) {
      return false;
    }
    console.log('handleSubmit', ds.current?.toData(), searchConditionDs.toJSONData());
    return false;
  };
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [modal]);
  return (
    <div className={prefixCls}>
      <Form dataSet={ds}>
        <TextField name="name" maxLength={10} />
        <FastSearchForm dataSet={searchConditionDs} />
        <TextField name="description" maxLength={30} />
      </Form>
    </div>
  );
};
const openCreateFastSearch = () => {
  Modal.open({
    key: Modal.key(),
    title: '创建快速筛选',
    style: {
      width: 740,
    },
    className: 'c7nagile-fastSearch-create',
    drawer: true,
    okText: '创建',
    cancelText: '取消',
    children: (
      <CreateFastSearch />
    ),
  });
};
export default openCreateFastSearch;
