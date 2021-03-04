import React, { useCallback } from 'react';
import {
  Draggable, Droppable,
} from 'react-beautiful-dnd';
import { List, AutoSizer, ListRowProps } from 'react-virtualized';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import ReactDOM from 'react-dom';
import classnames from 'classnames';
import DraggableItem from './DraggableItem';
import { useSortTableContext } from './stores';

interface Props {
  isDropDisabled: boolean,
  rows: Array<Record>,
}
const DropContent: React.FC<Props> = ({ isDropDisabled, rows }) => {
  const { showSplitLine, prefixCls } = useSortTableContext();
  const renderRowItem = useCallback((rowProps: ListRowProps) => {
    const record = rows[rowProps.index];
    return (
      <Draggable draggableId={String(record.key)} index={rowProps.index} key={record.key} isDragDisabled={isDropDisabled}>
        {(provided: any, snapshot: any) => (
          <DraggableItem
            provided={provided}
            data={rows[rowProps.index]}
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
      renderClone={(provided: any, snapshot: any, rubric: { source: { index: string | number; }; }) => (
        <DraggableItem
          draggingClassName={`${prefixCls}-dragging-item`}
          provided={provided}
          // @ts-ignore
          data={rows[rubric.source.index]}
        />
      )}
    >
      {(provided: { innerRef: (arg0: HTMLElement) => void; }, snapshot: any) => {
        const rowCount = rows.length;
        return (
          <AutoSizer>
            {({ width, height }) => (
              <List
                // autoHeight
                height={height}
                overscanRowCount={10}
                noRowsRenderer={() => <div className={classnames(`${prefixCls}-drop-null`, { [`${prefixCls}-drop-null-split`]: showSplitLine })}>暂无数据</div>}
                rowCount={rowCount}
                // onScroll={onChildScroll}
                rowHeight={showSplitLine ? 40 : 32}
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
                      provided.innerRef(whatHasMyLifeComeTo);
                    }
                  }
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
