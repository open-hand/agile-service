import { commonApi } from '@/api';
import { useQuery, UseQueryOptions } from 'react-query';
import useProjectKey from './useProjectKey';

export interface ParentArtDoingConfig {
  projectId?: string
}
export default function useParentArtDoing(config?: ParentArtDoingConfig, options?: UseQueryOptions<any>) {
  const { projectId } = config || {};

  const key = useProjectKey({ key: ['parent-art-doing'], projectId });
  return useQuery(key, () => commonApi.getIsShowFeature(), {
    staleTime: 10000,
    ...options,
  });
}
