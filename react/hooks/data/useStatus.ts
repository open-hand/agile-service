import { statusApi } from '@/api';
import { useQuery, UseQueryOptions, QueryKey } from 'react-query';
import { IStatus } from '@/common/types';

export interface ProjecStatustKeyConfig {
  key: QueryKey
  issueTypeId?: string
}
function useProjectStatusKey(config: ProjecStatustKeyConfig): QueryKey {
  return [config.key, {
    type: 'project',
    issueTypeId: config.issueTypeId,
  }];
}

export interface StatusConfig {
  applyType?: 'agile' | 'program'
  /** 只返回某一工作项类型的状态 */
  issueTypeId?: string
}
export default function useProjectStatus(config?: StatusConfig, options?: UseQueryOptions<IStatus[]>) {
  const key = useProjectStatusKey({ key: ['statusList'], issueTypeId: config?.issueTypeId });
  return useQuery(key, () => (config?.issueTypeId ? statusApi.loadAllForIssueType(config?.issueTypeId, config?.applyType) : statusApi.loadByProject(config?.applyType)), {
    initialData: [],
    ...options,
  });
}
