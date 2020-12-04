import React, { useMemo, useState, useCallback } from 'react';
import {
  Button, Modal, Spin, Tooltip,
} from 'choerodon-ui/pro';
import Measure from 'react-measure';
import { observer, Observer, useComputed } from 'mobx-react-lite';
import STATUS from '@/constants/STATUS';
import { IStatusCirculation } from '@/api';
import Table, { ColumnsType } from 'antd/lib/table';
import 'antd/lib/table/style';
import { useStateMachineContext } from '@/routes/StateMachine/context';
import CheckboxAll from './check-box-all';
import { useStatusCirculationContext } from '../..';
import Checkbox from './Checkbox';
import DeleteStatus from './DeleteStatus';
import styles from './index.less';

const StatusCirculationTable: React.FC = () => {
  const [height, setHeight] = useState<number>(0);
  const { store } = useStatusCirculationContext();
  const { selectedType } = useStateMachineContext();
  const { statusList, loading } = store;
  function judgeRowCheckAll(statusId: string, record: IStatusCirculation): boolean {
    const statusActions = store.actions.get(statusId);
    if (statusActions && statusActions.some((item) => item.type === 'nocheck')) {
      return false;
    }
    if (record.canTransformStatus.length !== statusList.length) {
      return false;
    }
    return true;
  }
  function judgeRowCheckSome(statusId: string, record: IStatusCirculation): boolean { /**   */
    const statusActions = store.actions.get(statusId);
    if (statusActions && statusActions.length > 0 && statusActions.some((item) => item.type === 'check')) {
      return true;
    }
    if (record.canTransformStatus.length === statusList.length) {
      return true;
    }
    return false;
  }
  const handleDeleteClick = useCallback((record: IStatusCirculation) => {
    Modal.open({
      title: `确认删除状态“${record.name}”`,
      children: <DeleteStatus
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
    title: (
      <span style={{ color: STATUS[status.type], fontWeight: 500 }}>
        <Observer>
          {() => {
            // eslint-disable-next-line react-hooks/rules-of-hooks
            const currentSize = useComputed(() => store.checkedMaps.get(status.id)?.columnCurrentSize, [store.checkedMaps.get(status.id)?.columnCurrentSize]);
            console.log(`render column ${status.name} -- size:${currentSize}`);
            return (
              <CheckboxAll
                name={`column-${status.name}`}
                // indeterminate=
                record={status}
                indeterminate={!!(currentSize && currentSize > 1 && currentSize < store.statusList.length)}
                checked={currentSize === store.statusList.length}
                onChange={(value) => {
                  store.statusList.forEach((item) => {
                    if (item.id !== status.id) {
                      store.checkChange(item.id, status.id, value);
                    }
                  });
                  return value;
                }}
              />
            );
          }}

        </Observer>
        {status.name}
      </span>),
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
      <span style={{ color: STATUS[record.type], fontWeight: 500 }}>
        <Observer>
          {() => {
            // eslint-disable-next-line react-hooks/rules-of-hooks
            const rowInfo = useComputed(() => store.checkedMaps.get(record.id)!, [store.checkedMaps.get(record.id)]);
            console.log(`render row ${record.name} -- checked:${rowInfo.rowChecked} indeterminate ${rowInfo.rowIndeterminate}`);

            return (
              <CheckboxAll
                name={`row-${record.name}`}
                record={record}
                indeterminate={rowInfo.rowIndeterminate}
                checked={rowInfo.rowChecked}
                onChange={(value) => {
                  store.statusList.forEach((item) => {
                    if (item.id !== record.id) {
                      store.checkChange(record.id, item.id, value);
                    }
                  });
                  return value;
                }}
              />
            );
          }}
        </Observer>
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
    width: 60,
    fixed: 'right',
    align: 'center',
    title: null,
    render: ((text: string, record) => {
      let disabled: [boolean, null | string] = [false, null];
      if (record.hasIssue) {
        disabled = [true, '该状态已被使用，不可删除'];
      }
      if (record.defaultStatus) {
        disabled = [true, '初始状态不可删除'];
      }
      return (
        <div>
          <Tooltip title={disabled[1]}>
            <Button
              disabled={disabled[0]}
              icon="delete"
              onClick={() => handleDeleteClick(record)}
            />
          </Tooltip>
        </div>
      );
    }

    ),
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
        <div ref={measureRef} className={styles.table}>
          <Spin spinning={loading}>
            <Table
              size="small"
              dataSource={data}
              scroll={{ x: 'max-content', y: height - 50 }}
              columns={statusColumns.length > 0 ? columns : []}
              pagination={false}
              locale={{
                emptyText: '暂无数据',
              }}
            />
          </Spin>
        </div>
      )}
    </Measure>
  );
};
export default observer(StatusCirculationTable);
