import { issueTypeApi } from '@/api';
import { IIssueType } from '@/common/types';
import { useQuery, UseQueryOptions } from 'react-query';
import useOrganizationKey from './useOrganizationKey';

export interface OrganizationIssueTypesConfig {
  organizationId?: string
  /** 只返回某一类的工作项类型 */
  typeCode?: string | string[]
}
export default function useOrganizationIssueTypes(config?: OrganizationIssueTypesConfig, options?: UseQueryOptions<IIssueType[]>) {
  const key = useOrganizationKey({ key: 'orgIssueTypes', organizationId: config?.organizationId });
  return useQuery(key, () => issueTypeApi.orgLoad({ params: { page: 0, size: 0 }, data: {} }), {
    select: (res) => {
      const issueTypes = ((Array.isArray(res) ? res : res.list) || []).filter((item: IIssueType) => item.typeCode !== 'backlog');
      // eslint-disable-next-line no-nested-ternary
      const typeCodes = Array.isArray(config?.typeCode) ? config?.typeCode : (config?.typeCode ? [config?.typeCode] : null);
      return typeCodes ? issueTypes.filter((type: IIssueType) => typeCodes.includes(type.typeCode)) : issueTypes;
    },
    initialData: [] as IIssueType[],
    ...options,
  });
}
