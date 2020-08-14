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
import DraggableItem from './DraggableItem';

interface Props {
  isDropDisabled: boolean,
  rows: Array<Record>,
}
const DropContent: React.FC<Props> = ({ isDropDisabled, rows }) => {
  const renderRowItem = useCallback((rowProps: ListRowProps) => {
    const record = rows[rowProps.index];
    return (
      <Draggable draggableId={String(record.id)} index={rowProps.index} key={record.id}>
        {(provided) => (
          <DraggableItem
            provided={provided}
            data={rows[rowProps.index]}
            virtualizedStyle={rowProps.style}
          />
        )}
      </Draggable>

    );
  }, [rows]);
  return (
    <Droppable
      droppableId="list-001"
      mode="virtual"
      isDropDisabled={isDropDisabled}
      renderClone={(provided, snapshot, rubric) => (
        <DraggableItem
          provided={provided}
          data={rows[rubric.source.index]}
        />
      )}
    >
      {(provided, snapshot) => {
        const rowCount = rows.length;
        return (
          <div
            ref={provided.innerRef}
          >
            <WindowScroller scrollElement={document.getElementsByClassName('c7n-page-issue-detail-content')[0]}>
              {({
                height, scrollTop,
                // @ts-ignore
                registerChild,
              }) => (
                <AutoSizer disableHeight>
                  {({ width }) => (
                    <div ref={(el) => registerChild(el)} style={{ width: '100%' }}>
                      <List
                        autoHeight
                        height={height}
                        noRowsRenderer={() => <div>暂无数据</div>}
                        rowCount={rowCount}
                        rowHeight={32}

                        rowRenderer={renderRowItem}
                        scrollTop={scrollTop}
                        width={width}
                        style={{
                          // background: snapshot.isDraggingOver ? '#e9e9e9' : 'inherit',
                          transition: 'background-color 0.2s ease',
                        }}
                      />
                    </div>
                  )}
                </AutoSizer>
              )}
            </WindowScroller>

          </div>
        );
      }}
    </Droppable>
  );
};
export default observer(DropContent);
