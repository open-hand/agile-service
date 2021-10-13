import React, { createContext } from 'react';
import type { IGanttDimensionTypeValue } from '.';
import type GanttStore from './store';

interface GanttContext {
  store: GanttStore
  searchFilter: any
  dimensionType:IGanttDimensionTypeValue
}
const context = createContext({} as GanttContext);
export default context;
