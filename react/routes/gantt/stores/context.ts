import React, { createContext, useContext } from 'react';
import { IssueSearchStore } from '@/components/issue-search';
import { IGanttSortLabelSortItem } from '../components/gantt-sort-label';
import type GanttStore from './store';
import type { IGanttDimensionTypeValue } from '../components/gannt-select/SelectType';
import { TableCacheRenderProps } from '@/components/table-cache';
import { IPersonalFilter } from '@/components/quick-search';
import { IHeaderFullScreenButtonComponentProps } from '@/hooks/useHeaderButtons/useHeaderFullScreenButton';

export interface IGanttProps extends TableCacheRenderProps {
  fullButtonProps?:IHeaderFullScreenButtonComponentProps
  isInProgram: boolean
  /** 组织层禁止编辑 */
  menuType: 'project' | 'org'
  myDefaultFilter: IPersonalFilter | undefined
  projectId?: string
  projects?: any[]
  setCurrentProject?: any
}
interface GanttContext extends IGanttProps {
  store: GanttStore
  // searchFilter: any
  menuType: IGanttProps['menuType']
  disable: boolean
  // processType: 'task' | 'workTime'
  issueSearchStore: IssueSearchStore
  projectId?: string
  // dimensionType: IGanttDimensionTypeValue
  // sortedList: IGanttSortLabelSortItem[]
}
const context = createContext({} as GanttContext);
function useGanttContext() {
  return useContext(context);
}
export { useGanttContext };
export default context;
