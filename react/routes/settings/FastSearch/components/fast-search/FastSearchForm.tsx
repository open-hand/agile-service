import React, { memo } from 'react';
import {
  DataSet, Form, Select, Button,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import FastSearchFormItemField from './FastSearchFormItemField';
import './index.less';

interface FastSearchFormItemProps {
  record: Record
  onDelete: (record: Record) => void
}

function FastSearchFormItemOrigin({ record, onDelete }: FastSearchFormItemProps) {
  const prefixCls = 'c7n-agile-fast-search-form-item';

  return (
    <div className={prefixCls}>
      <Form record={record} columns={5}>
        {record.index ? <Select name="bothRelation" colSpan={1} style={{ width: 100 }} /> : null}
        <Select name="attribute" colSpan={2} searchable searchMatcher={({ record: attributeRecord, text }) => String(attributeRecord.get('name')).toLocaleLowerCase().indexOf(text.toLocaleLowerCase()) !== -1} />
        <Select name="relation" colSpan={1} style={{ width: 100 }} />
        <FastSearchFormItemField name="value" colSpan={2} record={record} />
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
  const prefixCls = 'c7n-agile-fast-search-form';
  return (
    <div className={prefixCls}>
      {dataSet.map((record) => <FastSearchFormItem record={record} onDelete={(r) => dataSet.delete(r, false)} key={`FastSearchFormItem-${record.id}`} />)}
      <Button icon="add" onClick={() => dataSet.create()} color={'primary' as any} style={{ width: 'max-content' }}>添加筛选</Button>
    </div>
  );
}
export default observer(FastSearchForm);
