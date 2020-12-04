import { createContext } from 'react';
import type GanttStore from './store';

interface GanttContext {
  store: GanttStore
}
const context = createContext({} as GanttContext);
export default context;
