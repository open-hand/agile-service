import { useQuery, UseQueryOptions } from 'react-query';
import { Priority } from '@/common/types';
import { priorityApi } from '@/api';
import useProjectKey from './useProjectKey';

export interface IssueTypeConfig {
  projectId?: string

}
export default function useDefaultPriority(config?: IssueTypeConfig, options?: UseQueryOptions<Priority>) {
  const key = useProjectKey({ key: ['defaultPriority'], projectId: config?.projectId });
  return useQuery(key, () => priorityApi.project(config?.projectId).getDefaultByProject(), {
    ...options,
  });
}
