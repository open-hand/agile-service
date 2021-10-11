import { commonApi } from '@/api';
import { useQuery, UseQueryOptions } from 'react-query';
import useProjectKey from './useProjectKey';

export interface ISubProject {
  projectId: string,
  projName: string
  iamgeUrl: string
  creationDate: string
}

export interface SubProjectsConfig {
  projectId?: string
  /** 只返回某一类的工作项类型 */
  typeCode?: string | string[]
  /** 只查询启用的 */
  onlySelectEnable?: boolean
}
export default function useSubProjects(config?: SubProjectsConfig, options?: UseQueryOptions<ISubProject[]>) {
  const key = useProjectKey({ key: ['subProjects', { onlySelectEnable: config?.onlySelectEnable }], projectId: config?.projectId });
  return useQuery(key, () => commonApi.getSubProjects(config?.onlySelectEnable, config?.projectId), {
    initialData: [] as ISubProject[],
    ...options,
  });
}
