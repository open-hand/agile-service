import React, { createContext, useContext } from 'react';
import type { IGanttDimensionTypeValue, IGanttPageProps } from './Gantt';
import type GanttStore from './store';

interface GanttContext {
  store: GanttStore
  searchFilter: any
  menuType: IGanttPageProps['menuType']
  disable: boolean
  dimensionType: IGanttDimensionTypeValue
}
const context = createContext({} as GanttContext);
function useGanttContext() {
  return useContext(context);
}
export { useGanttContext };
export default context;
