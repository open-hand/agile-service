import { createContext } from 'react';
import type GanttStore from './store';
import { Gantt } from './types';

interface GanttContext {
  store: GanttStore
  getBarColor?: (item: Gantt.Item) => { backgroundColor: string, borderColor: string }
}
const context = createContext({} as GanttContext);
export default context;
