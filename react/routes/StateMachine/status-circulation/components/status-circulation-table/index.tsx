import React, { ReactNode, useMemo } from 'react';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import STATUS from '@/constants/STATUS';
import Table from '../table';
import { IStatusCirculation } from '../../StatusCirculationStore';
import styles from './index.less';
import './Checkbox.less';
import { useStatusCirculationContext } from '../..';

interface ColumnProps {
  name: string,
  lock?: boolean | 'right',
  renderHeader?: () => ReactNode | null,
  renderer?: ((record: Object) => ReactNode),
}
const StatusCirculationTable: React.FC = () => {
  const { store } = useStatusCirculationContext();
  const { statusList } = store;
  const data = useMemo(() => statusList.map((from) => statusList.reduce((result, to) => ({
    ...result,
    ...from,
    [to.id]: from.to.includes(to.id),
  }), {})), [statusList]);
  const columns: ColumnProps[] = useMemo(() => [{
    name: 'name',
    lock: true,
    renderHeader: () => null,
    renderer: ((record: IStatusCirculation) => (
      <span style={{ color: STATUS[record.valueCode] }}>{record.name}</span>
    )),
  }, {
    name: 'operate',
    lock: true,
    renderHeader: () => null,
    renderer: (() => '可流转到'),
  },
  ...statusList.map((status) => ({
    name: status.name,
    renderHeader: () => <span style={{ color: STATUS[status.valueCode] }}>{status.name}</span>,
    renderer: ((record: IStatusCirculation) => (
      <div className="md-checkbox">
        <input
          type="checkbox"
          disabled={status.id === record.id}
          checked={record[status.id]}
          onChange={(e) => {
            console.log(e.target.checked);
          }}
          id={`checkbox-${status.id}-${record.id}`}
        />
        {/* eslint-disable-next-line jsx-a11y/label-has-associated-control */}
        <label htmlFor={`checkbox-${status.id}-${record.id}`} />
      </div>
    )),
  })),
  {
    name: 'delete',
    lock: 'right',
    renderHeader: () => null,
    renderer: ((record: IStatusCirculation) => (record.default
      ? <span className={styles.default_status}>初始状态</span>
      : (
        <div>
          <Button icon="delete" />
        </div>
      )
    )),
  }], [statusList]);

  return <Table data={data} columns={columns} />;
};
export default observer(StatusCirculationTable);
