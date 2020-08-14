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
import { IFiledProps, pageConfigApi } from '@/api';
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
  const { sortTableDataSet, pageIssueTypeStore } = usePageIssueTypeStore();
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
            dataSet?.delete(record as Record);
        }}
      >
        <Icon type="delete" style={{ fontSize: 18 }} />
      </Button>
    </div>
  );

  const onDragStart = (initial: DragStart, provided: ResponderProvided) => {

  };
  const onDragEnd = (result: DropResult, provided: ResponderProvided) => {
    console.log('result', result, provided);
    const { destination, source } = result;
    if (!destination?.index) {
      return;
    }
    if (destination.index === source.index) {
      return;
    }

    const sourceRecord = sortTableDataSet.records[source.index];
    const destinationRecord = sortTableDataSet.records[destination.index];
    const rankObj = {
      before: false,
      issueType: pageIssueTypeStore.currentIssueType,
      previousRank: null,
      nextRank: null,
    };
    if (destination.index > source.index) {
      console.log('down');
      rankObj.previousRank = destinationRecord.get('rank');
    } else {
      console.log('up');
      rankObj.nextRank = destinationRecord.get('rank');
    }
    // console.log(sourceRecord.toData(), 'sourceRecord ', destinationRecord.toData());
    pageConfigApi.loadRankValue(rankObj).then((newRank:string) => {
      sourceRecord.set('rank', newRank);
    }).then(() => {
      sortTableDataSet.move(source.index, destination.index);
    });
  };
  return (
    <div className={prefixCls}>
      <div className={`${prefixCls}-header`}>
        {[...sortTableDataSet.fields.entries()].map((item: any) => {
          const itemProps = item[1].props;
          return <span className={`${prefixCls}-header-item`}>{itemProps.label || itemProps.name}</span>;
        })}
      </div>
      <div className={`${prefixCls}-content`}>
        <DragDropContext onDragEnd={onDragEnd} onDragStart={onDragStart}>
          <div style={{ width: '100%' }}>
            <DropContent rows={sortTableDataSet.records} isDropDisabled={false} />
          </div>
        </DragDropContext>
      </div>

    </div>
  );
};
export default observer(SortTable);
