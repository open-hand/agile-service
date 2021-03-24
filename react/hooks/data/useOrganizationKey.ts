import { getOrganizationId } from '@/utils/common';
import { QueryKey } from 'react-query';

export interface OrganizationKeyConfig {
  key: QueryKey
  organizationId?: string
}
export default function useOrganizationKey(config: OrganizationKeyConfig): QueryKey {
  return [config.key, {
    type: 'project',
    organizationId: config.organizationId ?? getOrganizationId(),
  }];
}
