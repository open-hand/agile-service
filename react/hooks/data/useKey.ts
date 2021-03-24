import { getIsOrganization, getProjectId, getOrganizationId } from '@/utils/common';

import { QueryKey } from 'react-query';

export interface keyConfig {
  key: QueryKey
  id?: string
}
export default function useKey(config: keyConfig): QueryKey {
  const isOrganization = getIsOrganization();
  return [config.key, {
    type: isOrganization ? 'organization' : 'project',
    projectOrOrgId: config.id ?? isOrganization ? getOrganizationId() : getProjectId(),
  }];
}
