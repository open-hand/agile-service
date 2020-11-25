import React, { useMemo } from 'react';
import Context from './context';
import GanttStore from './store';
import TimeAxis from './components/time-axis';
import TableHeader from './components/table-header';
import TableBody from './components/table-body';
import ScrollIndicator from './components/scroll-indicator';
import SelectionIndicator from './components/selection-indicator';
import TimeAxisScaleSelect from './components/time-axis-scale-select';
import TimeIndicator from './components/time-indicator';
import Chart from './components/chart';
import styles from './Gantt.less';

interface GanttProps {

}
const Gantt: React.FC<GanttProps> = () => {
  const store = useMemo(() => new GanttStore(), []);

  return (
    <Context.Provider value={{ store }}>
      <div className={styles.body}>
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
        <TimeIndicator />
        <TimeAxisScaleSelect />
      </div>
    </Context.Provider>
  );
};
export default Gantt;
