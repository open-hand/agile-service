import React, { useMemo, useState } from 'react';
import { Button, Modal } from 'choerodon-ui/pro';
import Measure, { BoundingRect } from 'react-measure';
import { observer } from 'mobx-react-lite';
import STATUS from '@/constants/STATUS';
import { IStatusCirculation } from '@/api';
import Table, { ColumnsType } from 'antd/lib/table';
import 'antd/lib/table/style';
import { useStatusCirculationContext } from '../..';
import Checkbox from './Checkbox';
import styles from './index.less';

const StatusCirculationTable: React.FC = () => {
  const [dimensions, setDimensions] = useState<BoundingRect>({
    width: -1,
    height: -1,
  } as BoundingRect);
  const { store } = useStatusCirculationContext();
  const { statusList } = store;
  const handleDeleteClick = (record: IStatusCirculation) => {
    Modal.confirm({
      title: '确认删除',
    });
  };
  const { data } = store;
  const statusColumns:ColumnsType<IStatusCirculation> = useMemo(() => statusList.map((status) => ({
    dataIndex: status.name,
    width: 150,
    title: () => <span style={{ color: STATUS[status.type] }}>{status.name}</span>,
    render: ((text: string, record) => (
      <Checkbox store={store} status={status} record={record} />
    )),
  })), [statusList, store]);
  const columns: ColumnsType<IStatusCirculation> = useMemo(() => [{
    dataIndex: 'name',
    width: 150,
    fixed: true,
    title: () => null,
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
    title: () => null,
    render: ((text: string, record) => (
      <div>
        <Button disabled={record.defaultStatus} icon="delete" onClick={() => handleDeleteClick(record)} />
      </div>
    )),
  }], [statusColumns]);
  const { width, height } = dimensions;
  return (
    <Measure
      bounds
      onResize={(contentRect) => {
        setDimensions(contentRect.bounds as BoundingRect);
      }}
    >
      {({ measureRef }) => (
        <div ref={measureRef} style={{ height: '100%', width: '100%', overflow: 'hidden' }}>
          <Table
            size="small"
            dataSource={data}
            scroll={{ x: width, y: height - 60 }}
            columns={columns}
            loading={store.loading}
            pagination={false}
          />
        </div>
      )}
    </Measure>
  );
};
export default observer(StatusCirculationTable);
