import React, {
  useContext, useCallback, useEffect, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import Hammer from 'hammerjs';
import TaskBar from '../task-bar';
import DragPresent from '../drag-present';
import Context from '../../context';
import styles from './index.less';

const Chart: React.FC = () => {
  const { store } = useContext(Context);
  const chartRef = useRef<HTMLDivElement>(null);
  const {
    tableWidth, viewWidth, bodyScrollHeight, translateX, barList,
  } = store;
  const minorList = store.getMinorList();
  const handleMouseMove = useCallback((event: React.MouseEvent<HTMLDivElement>) => {
    event.persist();
    store.handleMouseMove(event);
  }, [store]);
  useEffect(() => {
    if (chartRef.current) {
      const chartHammer = new Hammer(chartRef.current);
      store.setChartHammer(chartHammer);
    }
  }, [store]);
  return (
    <div
      ref={chartRef}
      className={styles.chart}
      onWheel={store.handleWheel}
      onMouseMove={handleMouseMove}
      style={{
        left: tableWidth,
        width: viewWidth,
        height: bodyScrollHeight,
      }}
    >
      <svg
        className={styles['chart-svg-renderer']}
        xmlns="http://www.w3.org/2000/svg"
        version="1.1"
        width={viewWidth}
        height={bodyScrollHeight}
        viewBox={`${translateX} 0 ${viewWidth} ${bodyScrollHeight}`}
      >
        {minorList.map((item) => (item.isWeek ? (
          <g key={item.key} stroke="#f0f0f0">
            <path d={`M${item.left}.5,0 L${item.left},${bodyScrollHeight}`} />
            <rect
              fill="#F7F7F7"
              opacity="0.5"
              strokeWidth="0"
              x={item.left}
              y={0}
              width={item.width}
              height={bodyScrollHeight}
            />
          </g>
        ) : (
          <g key={item.label} stroke="#f0f0f0">
            <path d={`M${item.left}.5,0 L${item.left},${bodyScrollHeight}`} />
          </g>
        )))}
        <DragPresent />
      </svg>
      <div className={styles['render-chunk']} style={{ height: bodyScrollHeight, transform: `translateX(-${translateX}px` }}>
        {barList.map((bar) => (
          <TaskBar
            key={bar.label}
            data={bar}
          />
        ))}
      </div>
    </div>
  );
};
export default observer(Chart);
