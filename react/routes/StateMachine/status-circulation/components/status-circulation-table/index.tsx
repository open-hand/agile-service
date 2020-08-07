import React, { ReactNode, useMemo } from 'react';
import { Button, Modal } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import STATUS from '@/constants/STATUS';
import { IStatusCirculation } from '@/api';
import Table from '../table';
import { useStatusCirculationContext } from '../..';
import Checkbox from './Checkbox';
import styles from './index.less';

interface ColumnProps {
  name: string,
  lock?: boolean | 'right',
  renderHeader?: () => ReactNode | null,
  renderer?: ((record: Object) => ReactNode),
}
const StatusCirculationTable: React.FC = () => {
  const { store } = useStatusCirculationContext();
  const { statusList } = store;
  const handleDeleteClick = (record: IStatusCirculation) => {
    Modal.confirm({
      title: '确认删除',
    });
  };
  const { data } = store;
  const columns: ColumnProps[] = useMemo(() => [{
    name: 'name',
    lock: true,
    renderHeader: () => null,
    renderer: ((record: IStatusCirculation) => (
      <span style={{ color: STATUS[record.type] }}>
        {record.name}
        {record.defaultStatus && <span className={styles.default_status}>初始</span>}
      </span>
    )),
  }, {
    name: 'operate',
    lock: true,
    renderHeader: () => null,
    renderer: (() => '可流转到'),
  },
  ...statusList.map((status) => ({
    name: status.name,
    renderHeader: () => <span style={{ color: STATUS[status.type] }}>{status.name}</span>,
    renderer: ((record: IStatusCirculation) => (
      <Checkbox store={store} status={status} record={record} />
    )),
  })),
  {
    name: 'delete',
    lock: 'right',
    renderHeader: () => null,
    renderer: ((record: IStatusCirculation) => (
      <div>
        <Button disabled={record.defaultStatus} icon="delete" onClick={() => handleDeleteClick(record)} />
      </div>
    )),
  }], [statusList]);

  return (
    <Table data={data} columns={columns} />
  );
};
export default observer(StatusCirculationTable);
