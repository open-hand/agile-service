import { createContext } from 'react';
import type GanttStore from './store';
import { Gantt } from './types';

interface GanttContext {
  store: GanttStore
  getBarColor?: (item: Gantt.Item) => { backgroundColor: string, borderColor: string }
  showBackToday: boolean
  showUnitSwitch: boolean
  onRow?: {
    onClick: (item: Gantt.Item) => void
  }
}
const context = createContext({} as GanttContext);
export default context;
