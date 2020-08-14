/* eslint-disable no-param-reassign */
import React, {
  useMemo, ReactElement, useEffect, memo, useState, PropsWithChildren, useCallback,
} from 'react';
import {
  Draggable, Droppable, DroppableStateSnapshot,
} from 'react-beautiful-dnd';
import {
  WindowScroller, List, AutoSizer, ListRowProps, ListRowRenderer,
} from 'react-virtualized';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import ReactDOM from 'react-dom';
import DraggableItem from './DraggableItem';
import { usePageIssueTypeStore } from '../../stores';

interface Props {
  isDropDisabled: boolean,
  rows: Array<Record>,
}
const DropContent: React.FC<Props> = ({ isDropDisabled, rows }) => {
  const { pageIssueTypeStore } = usePageIssueTypeStore();
  const [scrollHeight, setScrollHeight] = useState<number>(300);
  const renderRowItem = useCallback((rowProps: ListRowProps) => {
    console.log('rows', rows.length);
    const record = rows[rowProps.index];
    return (
      <Draggable draggableId={String(record.key)} index={rowProps.index} key={record.key}>
        {(provided) => (
          <DraggableItem
            provided={provided}
            data={rows[rowProps.index]}
            index={rowProps.index}
            virtualizedStyle={{ ...rowProps.style }}
          />
        )}
      </Draggable>
    );
  }, [rows]);
  return (
    <Droppable
      droppableId="sort-table"
      mode="virtual"
      isDropDisabled={isDropDisabled}
      renderClone={(provided, snapshot, rubric) => (
        <DraggableItem
          provided={provided}
          index={rubric.source.index}
          // virtualizedStyle={{ margin: 0 }}
          data={rows[rubric.source.index]}
        />
      )}
    >
      {(provided, snapshot) => {
        const rowCount = rows.length;
        console.log('rowCount', rowCount);
        return (
          <AutoSizer>
            {({ width, height }) => (
              <List
                // autoHeight
                height={scrollHeight}
                overscanRowCount={10}
                noRowsRenderer={() => <div>暂无数据</div>}
                rowCount={rowCount}
                // onScroll={onChildScroll}
                rowHeight={32}
                rowRenderer={renderRowItem}
                // scrollTop={scrollTop}
                width={width}
                ref={(ref) => {
                  // react-virtualized has no way to get the list's ref that I can so
                  // So we use the `ReactDOM.findDOMNode(ref)` escape hatch to get the ref
                  if (ref) {
                    // eslint-disable-next-line react/no-find-dom-node
                    const whatHasMyLifeComeTo = ReactDOM.findDOMNode(ref);

                    if (whatHasMyLifeComeTo instanceof HTMLElement) {
                      const element = document.getElementsByClassName('c7n-page-issue-detail')[0];
                      setScrollHeight(element.clientHeight - 30);
                      provided.innerRef(whatHasMyLifeComeTo);
                    }
                  }
                }}
                style={{
                  // background: snapshot.isDraggingOver ? '#e9e9e9' : 'inherit',
                  transition: 'background-color 0.2s ease',
                }}
              />
            )}
          </AutoSizer>
        );
      }}
    </Droppable>
  );
};
export default observer(DropContent);
