import React, { useMemo, useState, useCallback } from 'react';
import {
  Button, Modal, Spin, Tooltip,
  Icon,
} from 'choerodon-ui/pro';

import Measure from 'react-measure';
import { observer, Observer, useComputed } from 'mobx-react-lite';
import { DragDropContextProvider } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';
import update from 'immutability-helper';

import Table, { ColumnsType } from 'antd/lib/table';
import STATUS from '@/constants/STATUS';
import { IStatusCirculation, statusTransformApi } from '@/api';
import 'antd/lib/table/style';
import { useStateMachineContext } from '@/routes/StateMachine/context';
import CheckboxAll from '@/components/check-box';
import { getIsOrganization } from '@/utils/common';
import { useStatusCirculationContext } from '../..';
import Checkbox from './Checkbox';
import DeleteStatus from './DeleteStatus';
import DragableBodyRow from './DragableBodyRow';
import styles from './index.less';
import { Loading } from '@/components';

const StatusCirculationTable: React.FC = () => {
  const [height, setHeight] = useState<number>(0);
  const { store } = useStatusCirculationContext();
  const { selectedType, readOnly } = useStateMachineContext();
  const { statusList, loading } = store;

  const handleDeleteClick = useCallback((record: IStatusCirculation) => {
    Modal.open({
      title: '确认删除',
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
            return (
              <CheckboxAll
                disabled={readOnly}
                name={`column-${status.name}`}
                // indeterminate=
                className={styles.check_all}
                record={status}
                indeterminate={!!(currentSize && currentSize > 1 && currentSize < store.statusList.length)}
                checked={currentSize === store.statusList.length}
                onChange={(value) => {
                  store.checkAllOrUnAll(value, selectedType, undefined, status.id);
                  return !value;
                }}
              />
            );
          }}

        </Observer>
        {status.name}
      </span>),
    render: ((text: string, record) => (
      <Checkbox store={store} status={status} record={record} disabled={readOnly} selectedType={selectedType} />
    )),
  })), [readOnly, selectedType, statusList, store]);
  // @ts-ignore
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
            return (
              <CheckboxAll
                disabled={readOnly}
                name={`row-${record.name}`}
                record={record}
                className={styles.check_all}
                indeterminate={rowInfo.rowIndeterminate}
                checked={rowInfo.rowChecked}
                onChange={(value) => {
                  store.checkAllOrUnAll(value, selectedType, record.id);
                  return !value;
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
    render: (() => <span className={styles.table_text}>可流转到</span>),
  },
  // @ts-ignore
  ...statusColumns,
  // @ts-ignore
  ...(readOnly ? [] : [{
    dataIndex: 'delete',
    width: 60,
    fixed: 'right',
    align: 'center',
    title: null,
    // @ts-ignore
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
              icon="delete_sweep-o"
              className={styles.del_btn}
              disabled={disabled[0]}
              style={{
                cursor: disabled[0] ? 'not-allowed' : 'pointer',
                color: disabled[0] ? 'rgba(87,102,121,0.50)' : 'var(--primary-color)',
              }}
              onClick={() => {
                if (disabled[0]) {
                  return;
                }
                handleDeleteClick(record);
              }}
            />
          </Tooltip>
        </div>
      );
    }
    ),
  }]),
  ], [handleDeleteClick, readOnly, selectedType, statusColumns, store]);

  const components = {
    body: {
      row: (props: any) => <DragableBodyRow {...props} readOnly={readOnly} />,
    },
  };

  const moveRow = useCallback((dragIndex, hoverIndex) => {
    const dragRow = data[dragIndex];
    update(data, {
      $splice: [
        [dragIndex, 1],
        [hoverIndex, 0, dragRow],
      ],
    });
    const down = dragIndex < hoverIndex;
    if (hoverIndex !== dragIndex && data[dragIndex] && data[hoverIndex]) {
      // @ts-ignore
      statusTransformApi[getIsOrganization() ? 'orgSortStatus' : 'sortStatus'](data[dragIndex].stateMachineId, {
        // @ts-ignore
        outSetId: data[hoverIndex].nodeId,
        before: !down,
        // @ts-ignore
        nodeId: data[dragIndex].nodeId,
      }).then(() => {
        store.getStatusList(selectedType);
      });
    }
  }, [data, selectedType, store]);

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
          <Loading loading={loading}>
            <Table
              key={selectedType}
              size="small"
              dataSource={data}
              scroll={{ x: 'max-content', y: height - 50 }}
              columns={statusColumns.length > 0 ? columns : []}
              pagination={false}
              locale={{
                emptyText: '暂无数据',
              }}
              components={components}
              // @ts-ignore
              onRow={(record, index) => ({
                index,
                moveRow,
              })}
            />
          </Loading>
        </div>
      )}
    </Measure>
  );
};

const ObserverStatusCirculationTable = observer(StatusCirculationTable);

export default ({ ...props }) => (
  <DragDropContextProvider backend={HTML5Backend}>
    <ObserverStatusCirculationTable {...props} />
  </DragDropContextProvider>
);
