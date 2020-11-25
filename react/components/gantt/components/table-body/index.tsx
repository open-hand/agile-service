import React, { useContext, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import Context from '../../context';
import styles from './index.less';
import { ROW_HEIGHT } from '../../constants';

const TableBody: React.FC = () => {
  const { store } = useContext(Context);
  const { columns } = store;
  const columnsWidth = store.getColumnsWidth;
  const barList = store.getBarList;
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
      {barList.map((bar) => (
        <div className={styles.row} style={{ height: ROW_HEIGHT }}>
          {columns.map((column, index) => (
            <div
              key={column.name}
              className={styles.cell}
              style={{
                width: columnsWidth[index],
                minWidth: column.minWidth,
              }}
            >
              {/* @ts-ignore */}
              <span className={styles.ellipsis}>{bar.task[column.name]}</span>
            </div>
          ))}
        </div>
      ))}
    </div>
  );
};
export default observer(TableBody);
