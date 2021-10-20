import { useQuery, UseQueryOptions, QueryKey } from 'react-query';
import { statusApi } from '@/api';
import { IStatus } from '@/common/types';
import { getProjectId } from '@/utils/common';

export interface ProjecStatustKeyConfig {
  key: QueryKey
  issueTypeId?: string
  projectId?: string
}
function useProjectStatusKey(config: ProjecStatustKeyConfig): QueryKey {
  return [config.key, {
    type: 'project',
    issueTypeId: config.issueTypeId,
    projectId: config.projectId || getProjectId(),
  }];
}

export interface StatusConfig {
  applyType?: 'agile' | 'program'
  /** 只返回某一工作项类型的状态 */
  issueTypeId?: string
  projectId?: string
}
export default function useProjectStatus(config?: StatusConfig, options?: UseQueryOptions<IStatus[]>) {
  const key = useProjectStatusKey({ key: ['statusList'], issueTypeId: config?.issueTypeId, projectId: config?.projectId });
  return useQuery(key, () => (config?.issueTypeId ? statusApi.project(config.projectId).loadAllForIssueType(config?.issueTypeId, config?.applyType) : statusApi.loadByProject(config?.applyType)), {
    initialData: [],
    ...options,
  });
}
