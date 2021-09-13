import { QueryKey } from 'react-query';
import { getProjectId } from '@/utils/common';

export interface ProjectKeyConfig {
  key: QueryKey
  projectId?: string
}
export default function useProjectKey(config: ProjectKeyConfig): QueryKey {
  return [config.key, {
    type: 'project',
    projectId: config.projectId ?? getProjectId(),
  }];
}
