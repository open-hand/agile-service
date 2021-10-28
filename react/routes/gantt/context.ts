import React, { createContext, useContext } from 'react';
import { IGanttSortLabelSortItem } from './components/gantt-sort-label';
import type { IGanttDimensionTypeValue, IGanttPageProps } from './Gantt';
import type GanttStore from './store';

interface GanttContext {
  store: GanttStore
  searchFilter: any
  menuType: IGanttPageProps['menuType']
  disable: boolean
  processType: 'task' | 'workTime'
  projectId?: string
  dimensionType: IGanttDimensionTypeValue
  sortedList: IGanttSortLabelSortItem[]
}
const context = createContext({} as GanttContext);
function useGanttContext() {
  return useContext(context);
}
export { useGanttContext };
export default context;
