import React, { useMemo, useRef, useEffect } from 'react';
import { useSize } from 'ahooks';
import Context from './context';
import GanttStore from './store';
import Divider from './components/divider';
import TimeAxis from './components/time-axis';
import TableHeader from './components/table-header';
import TableBody from './components/table-body';
import ScrollIndicator from './components/scroll-indicator';
import SelectionIndicator from './components/selection-indicator';
import TimeAxisScaleSelect from './components/time-axis-scale-select';
import TimeIndicator from './components/time-indicator';
import Chart from './components/chart';
import styles from './Gantt.less';
import { Gantt } from './types';

interface GanttProps {
  data: Gantt.Item[]
  columns: Gantt.Column[]
}
const GanttComponent: React.FC<GanttProps> = ({ data, columns }) => {
  const store = useMemo(() => new GanttStore(), []);
  const ref = useRef<HTMLDivElement>(null);
  const size = useSize(ref);
  useEffect(() => {
    store.syncSize(size);
  }, [size, store]);
  useEffect(() => {
    store.setData(data);
  }, [data, store]);
  useEffect(() => {
    store.setColumns(columns);
  }, [columns, store]);
  return (
    <Context.Provider value={{ store }}>
      <div className={styles.body} ref={ref}>
        {/* <ScrollIndicator /> */}
        <header>
          <TableHeader />
          <TimeAxis />
        </header>
        <main ref={store.mainElementRef}>
          <SelectionIndicator />
          <TableBody />
          <Chart />
        </main>
        <Divider />
        <TimeIndicator />
        <TimeAxisScaleSelect />
      </div>
    </Context.Provider>
  );
};
export default GanttComponent;
