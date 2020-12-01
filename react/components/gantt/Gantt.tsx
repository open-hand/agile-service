import React, {
  useMemo, useRef, useEffect, useContext, forwardRef, useImperativeHandle, MutableRefObject,
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
import ScrollTop from './components/scroll-top';
import styles from './Gantt.less';
import { Gantt } from './types';

const Body: React.FC = ({ children }) => {
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
  onUpdate: (item: Gantt.Item, startDate: string, endDate: string) => Promise<boolean>
  startDateKey?: string
  endDateKey?: string
  isRestDay?: (date: string) => boolean
  getBarColor?: (item: Gantt.Item) => { backgroundColor: string, borderColor: string }
  showBackToday?: boolean
  showUnitSwitch?: boolean
  unit?: Gantt.Sight
}
export interface GanttRef {
  backToday: () => void
}
const GanttComponent: React.FC<GanttProps> = forwardRef(({
  data, columns, onUpdate, startDateKey = 'startDate', endDateKey = 'endDate', isRestDay, getBarColor,
  showBackToday = true, showUnitSwitch = true, unit,
}, ref) => {
  const store = useMemo(() => new GanttStore(), []);
  useEffect(() => {
    store.setData(data, startDateKey, endDateKey);
  }, [data, endDateKey, startDateKey, store]);
  useEffect(() => {
    store.setColumns(columns);
  }, [columns, store]);
  useEffect(() => {
    store.setOnUpdate(onUpdate);
  }, [onUpdate, store]);
  useEffect(() => {
    if (isRestDay) {
      store.setIsRestDay(isRestDay);
    }
  }, [isRestDay, store]);
  useEffect(() => {
    if (unit) {
      store.switchSight(unit);
    }
  }, [unit, store]);
  useImperativeHandle(ref, (): GanttRef => ({
    backToday: () => store.scrollToToday(),
  }));
  return (
    <Context.Provider value={{
      store, getBarColor, showBackToday, showUnitSwitch,
    }}
    >
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
        {showBackToday && <TimeIndicator />}
        {showUnitSwitch && <TimeAxisScaleSelect />}
        <ScrollBar />
        <ScrollTop />
      </Body>
    </Context.Provider>
  );
});
export default GanttComponent;
