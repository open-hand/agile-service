import React from 'react';
import { observer } from 'mobx-react-lite';
import {
  DropResult, ResponderProvided, DragDropContext, DragStart, DragUpdate,
} from 'react-beautiful-dnd';
import { IFiledProps, pageConfigApi } from '@/api';
import classnames from 'classnames';
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
  { name: 'fieldName', label: '字段名称', type: 'common' },
  { name: 'defaultValue', label: '默认值', type: 'common' },
  { name: 'required', label: '必填', type: 'project' },
  { name: 'required', label: '必填（控制项目）', type: 'organization' },
  { name: 'edited', label: '加入到编辑页', type: 'common' },
  { name: 'created', label: '加入到创建页', type: 'common' },

];

const prefixCls = 'c7n-page-issue-detail';
const SortTable: React.FC = () => {
  const { sortTableDataSet, pageIssueTypeStore } = usePageIssueTypeStore();
  const { showSplitLine } = useSortTableContext();
  const type = showSplitLine ? 'organization' : 'project';
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
      // before: false,
      // issueType: pageIssueTypeStore.currentIssueType,
      previousRank: null,
      nextRank: null,
    };
    if (destination.index > source.index) {
      rankObj.previousRank = destinationRecord.get('rank');
      rankObj.nextRank = sortTableDataSet.length - 1 !== destination.index ? sortTableDataSet.data[destination.index + 1].get('rank') : null;
    } else {
      rankObj.previousRank = destination.index !== 0 ? sortTableDataSet.data[destination.index - 1].get('rank') : null;
      rankObj.nextRank = destinationRecord.get('rank');
    }
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
      <div className={classnames(`${prefixCls}-header `, { [`${prefixCls}-header-split`]: showSplitLine })}>
        {columns.filter((item) => item.type === 'common' || item.type === type).map((itemProps) => <span className={`${prefixCls}-header-item`}>{itemProps.label || itemProps.name}</span>)}
      </div>
      <div className={`${prefixCls}-content`}>
        <DragDropContext
          onDragEnd={onDragEnd}
          onDragStart={onDragStart}
        // onDragUpdate={(initial: DragUpdate, provided: ResponderProvided) => {
        //   console.log('initial', initial, provided);
        // }}
        >
          <div className={`${prefixCls}-drop-wrap`}>
            <DropContent rows={sortTableDataSet.data} isDropDisabled={false} />
          </div>
        </DragDropContext>
      </div>

    </div>
  );
};
export default observer(SortTable);
