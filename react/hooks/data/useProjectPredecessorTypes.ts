import { useQuery, UseQueryOptions } from 'react-query';
import { ganttApi, IGanttPredecessorType } from '@/api';
import useProjectKey from './useProjectKey';

export interface ProjectIssueTypesConfig {
  projectId?: string
}
export default function useProjectPredecessorTypes(config?: ProjectIssueTypesConfig, options?: UseQueryOptions<IGanttPredecessorType[]>) {
  const key = useProjectKey({ key: ['predecessorTypes'], projectId: config?.projectId });
  return useQuery(key, () => ganttApi.project(config?.projectId).loadIssueDependencyTypes(), {
    initialData: [] as IGanttPredecessorType[],
    ...options,
  });
}
