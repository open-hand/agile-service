import {
  useQuery, UseQueryOptions,
} from 'react-query';
import {
  IssueLinkType,
  issueLinkTypeApi,
} from '@/api';
import useProjectKey from './useProjectKey';

export interface LinkTypeConfig {
  projectId?: string
  hasPassive?: boolean
}
export interface TransformedIssueLinkType {
  name: string,
  isIn: boolean,
  linkTypeId: string,
}
function transform(links: IssueLinkType[], hasPassive: boolean) {
  // split active and passive
  const active = links.map((link) => ({
    name: link.outWard,
    isIn: true,
    linkTypeId: link.linkTypeId,
  }));
  const passive: TransformedIssueLinkType[] = [];
  if (hasPassive) {
    links.forEach((link) => {
      if (link.inWard !== link.outWard) {
        passive.push({
          name: link.inWard,
          isIn: false,
          linkTypeId: link.linkTypeId,
        });
      }
    });
  }
  return active.concat(passive);
}
export function useLinkTypeKey(config: LinkTypeConfig) {
  return useProjectKey({ key: ['link-type'], projectId: config.projectId });
}
export default function useLinkType(config: LinkTypeConfig, options?: UseQueryOptions<TransformedIssueLinkType[]>) {
  const key = useLinkTypeKey(config);
  return useQuery(key, () => issueLinkTypeApi.getAll({}, config.projectId as any).then((res) => transform(res.list, config.hasPassive ?? true)), {
    ...options,
  });
}
