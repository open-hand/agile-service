import { useQuery, UseQueryOptions } from 'react-query';
import { commonApi } from '@/api';
import useProjectKey from './useProjectKey';

export interface ParentArtDoingConfig {
  projectId?: string
}
export default function useParentArtDoing(config?: ParentArtDoingConfig, options?: UseQueryOptions<any>) {
  const { projectId } = config || {};

  const key = useProjectKey({ key: ['parent-art-doing'], projectId });
  return useQuery(key, () => commonApi.project(projectId).getIsShowFeature(), {
    ...options,
  });
}
