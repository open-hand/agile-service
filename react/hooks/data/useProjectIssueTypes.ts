import { issueTypeApi } from '@/api';
import { IIssueType } from '@/common/types';
import { useEffect } from 'react';
import { useQuery, UseQueryOptions } from 'react-query';
import useIsProgram from '../useIsProgram';
import useProjectKey from './useProjectKey';

export interface ProjectIssueTypesConfig {
  applyType?: 'agile' | 'program'
  projectId?: string
  /** 只返回某一类的问题类型 */
  typeCode?: string | string[]
  /** 只查询启用的 */
  onlyEnabled?: boolean
  programId?: string | number
  isProgram?: boolean
}
export default function useProjectIssueTypes(config?: ProjectIssueTypesConfig, options?: UseQueryOptions<IIssueType[]>) {
  const { isProgram } = useIsProgram();
  const key = useProjectKey({ key: ['issueTypes', { onlyEnabled: config?.onlyEnabled, isProgram: config?.isProgram ?? isProgram }], projectId: config?.projectId });
  return useQuery(key, () => issueTypeApi.loadAllWithStateMachineId('agile', config?.projectId, config?.onlyEnabled, config?.programId), {
    select: (data) => {
      const issueTypes = (!(config?.isProgram ?? isProgram) ? data.filter((item: IIssueType) => item.typeCode !== 'feature') : data);
      // eslint-disable-next-line no-nested-ternary
      const typeCodes = Array.isArray(config?.typeCode) ? config?.typeCode : (config?.typeCode ? [config?.typeCode] : null);
      return typeCodes ? issueTypes.filter((type) => typeCodes.includes(type.typeCode)) : issueTypes;
    },
    initialData: [] as IIssueType[],
    ...options,
  });
}
