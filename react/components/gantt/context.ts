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
  tableIndent: number
  expandIcon?: ({ level, collapsed, onClick }: {
    level: number,
    collapsed: boolean
    onClick: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  }) => React.ReactNode
}
const context = createContext({} as GanttContext);
export default context;
