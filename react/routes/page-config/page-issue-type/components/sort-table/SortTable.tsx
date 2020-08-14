/* eslint-disable no-param-reassign */
import React, {
  useMemo, ReactElement, useEffect, memo, useState, PropsWithChildren,
} from 'react';
import { observer, useObservable } from 'mobx-react-lite';
import {
  Table, DataSet, Icon, CheckBox, Spin, Output, Button,
} from 'choerodon-ui/pro/lib';
import { TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { ColumnProps } from 'choerodon-ui/pro/lib/table/Column';
import {
  DropResult, ResponderProvided, DraggableProvided,
  DraggableStateSnapshot, DraggableRubric, DragDropContext, DragStart,
} from 'react-beautiful-dnd';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { IFiledProps } from '@/api';
import OldSortTable from '../../../page/components/SortTable';
import './index.less';
import { usePageIssueTypeStore } from '../../stores';
import { useSortTableContext } from './stores';
import DropContent from './DropContent';

const { Column } = Table;
interface Props {
  disabled: boolean | undefined,
  org?: number,
  dataStatus: { code: string },
  onDelete?: (data: IFiledProps) => void,
}
interface DragRenderCloneProps {
  provided: DraggableProvided, // DraggableProvided,
  snapshot: DraggableStateSnapshot, // DraggableStateSnapshot,
  rubric: DraggableRubric,
  key: any,
  record: Record,
  column: ColumnProps[],
}
const prefixCls = 'c7n-page-issue-detail';
const SortTable: React.FC = () => {
  const { sortTableDataSet } = usePageIssueTypeStore();
  const { disabled, dataStatus, onDelete } = useSortTableContext();
  // const recordMaps = useMemo(() =>
  //  new Map(sortTableDataSet.records.map(record => [record.id, record])), [sortTableDataSet])
  // const [dataStatus, setDataStatus] = useState<string>();
  const renderFieldName = ({ value }: RenderProps) => (
    <div>
      {!disabled && <Icon type="baseline-drag_indicator" />}
      <span>{value}</span>
    </div>
  );

  function renderCheckBox({
    value, name, record, dataSet,
  }: RenderProps) {
    return (
      <CheckBox
        disabled={disabled}
        checked={value}
        // value={value}
        onChange={(val) => {
          record?.set(name as String, val);
          // console.log('dataSet?.dirty', dataSet?.dirty);
          if (dataStatus.code !== 'drag_update' && dataSet?.dirty) {
            // setDataStatus('update');
            dataStatus.code = 'update';
          } else if (dataStatus.code !== 'drag_update' && !dataSet?.dirty) {
            // setDataStatus('ready');
            dataStatus.code = 'ready';
          }
        }}
      />
    );
    // return <input type="checkbox" checked={value} />;
  }
  const renderAction = ({
    value, name, record, dataSet,
  }: RenderProps) => (
    <div>
      {renderCheckBox({
        value, name, record, dataSet,
      })}
      <Button
        disabled={disabled}
        style={{ marginLeft: 10 }}
        onClick={() => {
          onDelete && onDelete(record?.toData());
          // dataSet?.delete(record as Record);
        }}
      >
        <Icon type="delete" style={{ fontSize: 18 }} />
      </Button>
    </div>
  );
  const getColumns = () => {
    console.log('co');
    return [
      {
        title: '字段名称',
        dataIndex: 'fieldName',
        width: '25%',
        render: (value: string) => (
          <div>
            {!disabled && <Icon type="baseline-drag_indicator" />}
            <span>{value}</span>
          </div>
        ),
      },
      {
        title: '默认值',
        dataIndex: 'defaultValue',
        width: '25%',
        // minWidth: '70px',
      },
      {
        title: '是否必填',
        dataIndex: 'require',
        width: '75px',
        render: (value: any) => (
          <CheckBox
            disabled={disabled}
            checked
            // value={value}
            onChange={(val) => {
              console.log('val', val);
              // record?.set(name as String, val);
            }}
          />
        ),
      },
      {
        title: '是否加入到编辑页',
        dataIndex: 'eidt',
        width: '135px',
        render: (value: any) => (
          <CheckBox
            disabled={disabled}
            checked
            value={value}
            onChange={(val) => {
              console.log('val', val);
              // record?.set(name as String, val);
            }}
          />
        ),
      },
      {
        title: '是否加入到创建页',
        dataIndex: 'create',
        width: '135px',
        render: (value: any) => (
          <CheckBox
            disabled={disabled}
            checked
            value={value}
            onChange={(val) => {
              console.log('val', val);
              // record?.set(name as String, val);
            }}
          />
        ),
      },
    ];
  };

  const onDragStart = (initial: DragStart, provided: ResponderProvided) => {

  };
  const onDragEnd = (result: DropResult, provided: ResponderProvided) => {
    console.log('result', result, provided);
    const { destination, source } = result;
    if (!destination?.index) {
      return;
    }
    const destinationData = sortTableDataSet.records[destination.index];
    const sourceData = sortTableDataSet.records[source.index];
    const destinationDataClone = destinationData.clone();
    const sourceDataClone = sourceData.clone();
    // [...sortTableDataSet.fields.entries()].forEach(((item: any) => {
    //   const itemProps = item[1].props;
    //   sourceData.set(itemProps.name, destinationDataClone.get(itemProps.name));
    //   destinationData.set(itemProps.name, sourceDataClone.get(itemProps.name));
    // }));
  };
  console.log('lo ', sortTableDataSet);
  return (
    <div className={prefixCls}>
      <div className={`${prefixCls}-header`}>
        {[...sortTableDataSet.fields.entries()].map((item: any) => {
          console.log('item:', item);
          const itemProps = item[1].props;
          return <span className={`${prefixCls}-header-item`}>{itemProps.label || itemProps.name}</span>;
        })}
      </div>
      <div className={`${prefixCls}-content`}>
        <DragDropContext onDragEnd={onDragEnd} onDragStart={onDragStart}>
          <DropContent rows={sortTableDataSet.records} isDropDisabled={false} />
        </DragDropContext>
      </div>

    </div>
  );
};
export default memo(observer(SortTable));
