import { includes } from 'lodash';
import { useQuery, UseQueryOptions } from 'react-query';
import { issueTypeApi } from '@/api';
import { IIssueType } from '@/common/types';
import { getIsOrganization } from '@/utils/common';
import useIsProgram from '../useIsProgram';
import useKey from './useKey';

export interface IssueTypeConfig {
  id?: string
  /** 只返回某一类的工作项类型 */
  typeCode?: string | string[]
  applyType?: 'agile' | 'program'
  onlyEnabled?: boolean
  hasTemplate?: boolean
  excludeTypes?: string[]
  programId?: string | number
}
export default function useIssueTypes(config?: IssueTypeConfig, options?: UseQueryOptions<IIssueType[]>) {
  const isOrganization = getIsOrganization();
  const { isProgram } = useIsProgram();
  const applyType = isProgram ? 'program' : 'agile';
  // eslint-disable-next-line no-nested-ternary
  const key = useKey({ key: [isOrganization ? (config?.hasTemplate ? 'orgHasTemplateIssueTypes' : 'orgIssueTypes') : 'projectIssueTypes', { onlyEnabled: config?.onlyEnabled }], id: config?.id });

  // eslint-disable-next-line no-nested-ternary
  return useQuery(key, () => (isOrganization ? (config?.hasTemplate ? issueTypeApi.orghasTemplateList() : issueTypeApi.orgLoad({ params: { page: 0, size: 0 }, data: {} })) : issueTypeApi.loadAllWithStateMachineId(config?.applyType ?? applyType, config?.id, config?.onlyEnabled, config?.programId)), {
    select: (res) => {
      let issueTypes = [];
      if (isOrganization) {
        issueTypes = ((Array.isArray(res) ? res : res.list) || []).filter((item: IIssueType) => item.typeCode !== 'backlog' && !includes(config?.excludeTypes || [], item.typeCode));
      } else {
        issueTypes = (!isProgram ? res.filter((item: IIssueType) => item.typeCode !== 'feature' && !includes(config?.excludeTypes || [], item.typeCode)) : res);
      }
      // eslint-disable-next-line no-nested-ternary
      const typeCodes = Array.isArray(config?.typeCode) ? config?.typeCode : (config?.typeCode ? [config?.typeCode] : null);
      return typeCodes ? issueTypes.filter((type: IIssueType) => typeCodes.includes(type.typeCode)) : issueTypes;
    },
    initialData: [] as IIssueType[],
    ...options,
  });
}
