import React, {
  useMemo, useRef, useEffect, useContext,
} from 'react';
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
import ScrollBar from './components/scroll-bar';
import Chart from './components/chart';
import styles from './Gantt.less';
import { Gantt } from './types';

const Body:React.FC = ({ children }) => {
  const { store } = useContext(Context);
  const ref = useRef<HTMLDivElement>(null);
  const size = useSize(ref);
  useEffect(() => {
    store.syncSize(size);
  }, [size, store]);
  return (
    <div className={styles.body} ref={ref}>
      {children}
    </div>
  );
};
interface GanttProps {
  data: Gantt.Item[]
  columns: Gantt.Column[]
}
const GanttComponent: React.FC<GanttProps> = ({ data, columns }) => {
  const store = useMemo(() => new GanttStore(), []);
  useEffect(() => {
    store.setData(data);
  }, [data, store]);
  useEffect(() => {
    store.setColumns(columns);
  }, [columns, store]);
  return (
    <Context.Provider value={{ store }}>
      <Body>
        {/* <ScrollIndicator /> */}
        <header>
          <TableHeader />
          <TimeAxis />
        </header>
        <main ref={store.mainElementRef} onScroll={store.handleScroll}>
          <SelectionIndicator />
          <TableBody />
          <Chart />
        </main>
        <Divider />
        <TimeIndicator />
        <TimeAxisScaleSelect />
        <ScrollBar />
      </Body>
    </Context.Provider>
  );
};
export default GanttComponent;
