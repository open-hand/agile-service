import { Priority } from '@/common/types';
import { useQuery, UseQueryOptions } from 'react-query';
import { priorityApi } from '@/api';
import useProjectKey from './useProjectKey';

export interface IssueTypeConfig {

}
export default function useDefaultPriority(config?: IssueTypeConfig, options?: UseQueryOptions<Priority>) {
  const key = useProjectKey({ key: ['defaultPriority'] });
  return useQuery(key, () => priorityApi.getDefaultByProject(), {
    ...options,
  });
}
