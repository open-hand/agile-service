import React, { memo, useCallback, useMemo } from 'react';
import {
  DataSet, Form, Select, Button, Row, Col,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import FastSearchFormItemField from './FastSearchFormItemField';
import styles from './FastSearchForm.less';

interface FastSearchFormItemProps {
  record: Record
  onDelete: (record: Record) => void
}
const { Option } = Select;
function FastSearchFormItemOrigin({ record, onDelete }: FastSearchFormItemProps) {
  const colSpanList = useMemo(() => {
    const spanList = [0, 10, 4, 10];
    if (record.index > 0) {
      spanList[0] = 4;
      spanList[1] -= spanList[0];
    }
    return spanList;
  }, [record.index]);

  return (
    <div className={styles.form_item}>
      <Form record={record} columns={5}>
        <Row>
          {colSpanList[0] > 0 && (
            <Col span={colSpanList[0]} style={{ paddingRight: '.1rem' }}>
              <Select name="bothRelation">
                <Option value="and">且</Option>
                <Option value="or">或</Option>
              </Select>
            </Col>
          )}
          <Col span={colSpanList[1]} style={{ paddingRight: '.1rem' }}>
            <Select name="attribute" style={{ width: '100%' }} clearButton={false} searchable searchMatcher={({ record: attributeRecord, text }) => String(attributeRecord.get('name')).toLocaleLowerCase().indexOf(text.toLocaleLowerCase()) !== -1} />
          </Col>
          <Col span={colSpanList[2]} style={{ paddingRight: '.1rem' }}>
            <Select name="relation" clearButton={false} />
          </Col>
          <Col span={colSpanList[3]}>
            <FastSearchFormItemField name="value" record={record} />
          </Col>
        </Row>
      </Form>
      <Button icon="delete" onClick={() => onDelete(record)} disabled={!record.index} className={styles.form_item_del_btn} />
    </div>
  );
}
const FastSearchFormItem = memo(FastSearchFormItemOrigin);

function FastSearchForm({ dataSet }: { dataSet: DataSet }) {
  const handleDelete = useCallback((record: Record) => { dataSet.delete(record, false); }, [dataSet]);
  // const handleCreate = useCallback(() => { dataSet.create(); }, [dataSet]);

  return (
    <div className={styles.form}>
      {dataSet.map((record) => <FastSearchFormItem record={record} onDelete={handleDelete} key={`FastSearchFormItem-${record.id}`} />)}
      <Button icon="add" onClick={() => dataSet.create()} color={'primary' as any} style={{ width: 'max-content' }}>添加筛选</Button>
    </div>
  );
}
export default observer(FastSearchForm);
