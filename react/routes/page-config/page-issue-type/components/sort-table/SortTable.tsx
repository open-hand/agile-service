import React from 'react';
import { observer } from 'mobx-react-lite';
import {
  DropResult, ResponderProvided, DragDropContext, DragStart,
} from 'react-beautiful-dnd';
import { IFiledProps, pageConfigApi } from '@/api';
import './index.less';
import { usePageIssueTypeStore } from '../../stores';
import { useSortTableContext } from './stores';
import DropContent from './DropContent';

interface Props {
  disabled: boolean | undefined,
  org?: number,
  dataStatus: { code: string },
  onDelete?: (data: IFiledProps) => void,
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
      // pageIssueTypeStore.setDataStatusCode(PageIssueTypeStoreStatusCode.drag);
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
