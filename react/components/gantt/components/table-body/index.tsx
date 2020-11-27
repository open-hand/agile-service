/* eslint-disable no-underscore-dangle */
import React, { useContext, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import classNames from 'classnames';
import Context from '../../context';
import styles from './index.less';
import { ROW_HEIGHT, TOP_PADDING } from '../../constants';
import RowToggler from './RowToggler';

const TableRows = () => {
  const { store } = useContext(Context);
  const { columns } = store;
  const columnsWidth = store.getColumnsWidth;
  const barList = store.getBarList;
  const { count, start } = store.getVisibleRows;
  return (
    <>
      {barList.slice(start, start + count).map((bar, rowIndex) => {
        // 父元素如果是其最后一个祖先的子，要隐藏上一层的线
        const parent = bar._parent;
        const parentItem = parent?._parent;
        let isLastChild = false;
        if (parentItem?.children) {
          if (parentItem.children[parentItem.children.length - 1] === bar._parent) {
            isLastChild = true;
          }
        }
        return (
          <div className={styles.row} style={{ height: ROW_HEIGHT, top: (rowIndex + start) * ROW_HEIGHT + TOP_PADDING }}>
            {columns.map((column, index) => (
              <div
                key={column.name}
                className={styles.cell}
                style={{
                  width: columnsWidth[index],
                }}
              >
                {index === 0 && (
                  Array(bar._depth).fill(0).map((_, i) => (
                    <div
                  // eslint-disable-next-line react/no-array-index-key
                      key={i}
                      className={classNames(styles['row-indentation'], {
                        [styles['row-indentation-hidden']]: isLastChild && i === bar._depth - 2,
                        [styles['row-indentation-both']]: i === bar._depth - 1,
                      })}
                    />
                  ))
                )}
                {index === 0 && bar._childrenCount > 0 && (
                <RowToggler
                  level={bar._depth}
                  collapsed={bar._collapsed}
                  onClick={() => {
                    store.setRowCollapse(bar.task, !bar._collapsed);
                  }}
                />
                )}
                {/* @ts-ignore */}
                <span className={styles.ellipsis}>{column.render ? column.render(bar.task) : bar.task[column.name]}</span>
              </div>
            ))}
          </div>
        );
      })}
    </>
  );
};
const ObserverTableRows = observer(TableRows);
const TableBody: React.FC = () => {
  const { store } = useContext(Context);
  const handleMouseMove = useCallback((event: React.MouseEvent<HTMLDivElement>) => {
    event.persist();
    store.handleMouseMove(event);
  }, [store]);
  const handleMouseLeave = useCallback(() => {
    store.handleMouseLeave();
  }, [store]);
  return (
    <div
      className={styles.scrollable}
      style={{
        width: store.tableWidth,
        height: store.bodyScrollHeight,
      }}
      onMouseMove={handleMouseMove}
      onMouseLeave={handleMouseLeave}
    >
      <ObserverTableRows />
    </div>
  );
};
export default observer(TableBody);
