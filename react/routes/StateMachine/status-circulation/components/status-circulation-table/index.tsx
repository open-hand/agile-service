import React, { useMemo, useState, useCallback } from 'react';
import { Button, Modal, Spin } from 'choerodon-ui/pro';
import Measure, { BoundingRect } from 'react-measure';
import { observer } from 'mobx-react-lite';
import STATUS from '@/constants/STATUS';
import { IStatusCirculation } from '@/api';
import Table, { ColumnsType } from 'antd/lib/table';
import 'antd/lib/table/style';
import { useStateMachineContext } from '@/routes/StateMachine/context';
import { useStatusCirculationContext } from '../..';
import Checkbox from './Checkbox';
import DeleteStatus from './DeleteStatus';
import styles from './index.less';

const StatusCirculationTable: React.FC = () => {
  const [height, setHeight] = useState<number>(0);
  const { store } = useStatusCirculationContext();
  const { selectedType } = useStateMachineContext();
  const { statusList, loading } = store;
  const handleDeleteClick = useCallback((record: IStatusCirculation) => {
    Modal.open({
      title: `确认删除状态“${record.name}”`,
      children: <DeleteStatus
        statusList={store.statusList.filter((status) => status.id !== record.id)}
        data={record}
        selectedType={selectedType}
        onSubmit={async () => {
          store.clearStatusActions(record.id);
          store.getStatusList(selectedType);
        }}
      />,
    });
  }, [selectedType, store]);
  const { data } = store;
  const statusColumns: ColumnsType<IStatusCirculation> = useMemo(() => statusList.map((status) => ({
    dataIndex: status.name,
    width: 150,
    title: <span style={{ color: STATUS[status.type] }}>{status.name}</span>,
    render: ((text: string, record) => (
      <Checkbox store={store} status={status} record={record} />
    )),
  })), [statusList, store]);
  const columns: ColumnsType<IStatusCirculation> = useMemo(() => [{
    dataIndex: 'name',
    width: 150,
    fixed: true,
    title: null,
    render: ((text: string, record) => (
      <span style={{ color: STATUS[record.type] }}>
        {record.name}
        {record.defaultStatus && <span className={styles.default_status}>初始</span>}
      </span>
    )),
  }, {
    dataIndex: 'operate',
    width: 100,
    fixed: true,
    title: () => null,
    render: (() => '可流转到'),
  },
  ...statusColumns,
  {
    dataIndex: 'delete',
    width: 80,
    fixed: 'right',
    align: 'center',
    title: null,
    render: ((text: string, record) => (
      <div>
        <Button disabled={record.defaultStatus} icon="delete" onClick={() => handleDeleteClick(record)} />
      </div>
    )),
  }], [handleDeleteClick, statusColumns]);
  return (
    <Measure
      bounds
      onResize={(contentRect) => {
        if (contentRect.bounds?.height !== height) {
          setHeight(contentRect.bounds?.height || 0);
        }
      }}
    >
      {({ measureRef }) => (
        <div ref={measureRef} style={{ height: '100%', width: '100%', overflow: 'hidden' }}>
          <Spin spinning={loading}>
            <Table
              size="small"
              dataSource={data}
              scroll={{ x: 'max-content', y: height - 50 }}
              columns={statusColumns.length > 0 ? columns : []}
              pagination={false}
            />
          </Spin>
        </div>
      )}
    </Measure>
  );
};
export default observer(StatusCirculationTable);
