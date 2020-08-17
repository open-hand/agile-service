/* eslint-disable no-param-reassign */
import React, {
  useMemo, ReactElement, useEffect, memo, useState, PropsWithChildren,
} from 'react';
import { observer, useObservable, useObserver } from 'mobx-react-lite';
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
import { PageIssueTypeStoreStatusCode } from '../../stores/PageIssueTypeStore';

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
const columns = [
  { name: 'fieldName', label: '字段名称' },
  { name: 'defaultValue', label: '默认值' },
  { name: 'required', label: '必填' },
  { name: 'edited', label: '加入到编辑页' },
  { name: 'created', label: '加入到创建页' },

];

const prefixCls = 'c7n-page-issue-detail';
const SortTable: React.FC = () => {
  const { sortTableDataSet, pageIssueTypeStore } = usePageIssueTypeStore();
  const { disabled, dataStatus, onDelete } = useSortTableContext();

  const onDragStart = (initial: DragStart, provided: ResponderProvided) => {

  };
  const onDragEnd = async (result: DropResult, provided: ResponderProvided) => {
    const { destination, source } = result;
    if (!destination) {
      return;
    }
    if (destination.index === source.index) {
      return;
    }

    const sourceRecord = sortTableDataSet.data[source.index];
    const destinationRecord = sortTableDataSet.data[destination.index];
    const rankObj = {
      before: false,
      issueType: pageIssueTypeStore.currentIssueType,
      previousRank: null,
      nextRank: null,
    };
    if (destination.index > source.index) {
      rankObj.previousRank = destinationRecord.get('rank');
    } else {
      rankObj.nextRank = destinationRecord.get('rank');
    }
    // 在异步函数中调用 防止不修改record
    // const outSetRank = (newRank: string) => sourceRecord.set('rank', newRank);
    sortTableDataSet.move(source.index, destination.index);
    await pageConfigApi.loadRankValue(rankObj).then((newRank: string) => {
      // sortTableDataSet.data[source.index].set('rank', newRank);
      // outSetRank(newRank);
      sourceRecord.set('rank', newRank);
      pageIssueTypeStore.setDataStatusCode(PageIssueTypeStoreStatusCode.drag);
    });
  };
  return (
    <div className={prefixCls}>
      <div className={`${prefixCls}-header`}>
        {columns.map((itemProps) => <span className={`${prefixCls}-header-item`}>{itemProps.label || itemProps.name}</span>)}
      </div>
      <div className={`${prefixCls}-content`}>
        <DragDropContext onDragEnd={onDragEnd} onDragStart={onDragStart}>
          <div style={{ width: '100%' }}>
            <DropContent rows={sortTableDataSet.data} isDropDisabled={false} />
          </div>
        </DragDropContext>
      </div>

    </div>
  );
};
export default observer(SortTable);
