import { useQuery, UseQueryOptions } from 'react-query';
import { commonApi } from '@/api';
import useProjectKey from './useProjectKey';

export interface ParentProgramConfig {
  projectId?: string
}
export default function useParentProgram(config?: ParentProgramConfig, options?: UseQueryOptions<any>) {
  const { projectId } = config || {};

  const key = useProjectKey({ key: ['parent-program'], projectId });
  return useQuery(key, () => {
    const promise = commonApi.getProjectsInProgram(projectId);
    promise.cancel = () => { };
    return promise;
  }, {
    ...options,
  });
}
