import { useQuery, UseQueryOptions } from 'react-query';
import { issueTypeApi } from '@/api';
import { IIssueType } from '@/common/types';
import useIsProgram from '../useIsProgram';
import useProjectKey from './useProjectKey';
import useIsInProgram from '../useIsInProgram';

export interface ProjectIssueTypesConfig {
  applyType?: 'agile' | 'program'
  projectId?: string
  /** 只返回某一类的问题类型 */
  typeCode?: string | string[]
  /** 只查询启用的 */
  onlyEnabled?: boolean
  programId?: string | number
  isProgram?: boolean
  isInProgram?: boolean
}
export default function useProjectIssueTypes(config?: ProjectIssueTypesConfig, options?: UseQueryOptions<IIssueType[]>) {
  const { isProgram } = useIsProgram();
  const { isInProgram } = useIsInProgram();
  const key = useProjectKey({ key: ['issueTypes', { onlyEnabled: config?.onlyEnabled ?? true, isProgram: config?.isProgram ?? isProgram }], projectId: config?.projectId });
  return useQuery(key, () => issueTypeApi.loadAllWithStateMachineId('agile', config?.projectId, config?.onlyEnabled ?? true, config?.programId), {
    select: (data) => {
      const issueTypes = (!(config?.isProgram ?? isProgram) ? data.filter((item: IIssueType) => item.typeCode !== 'feature') : data);
      const finalIssueTypes = (config?.isInProgram ?? isInProgram) ? issueTypes.filter((item) => item.typeCode !== 'issue_epic') : data;
      // eslint-disable-next-line no-nested-ternary
      const typeCodes = Array.isArray(config?.typeCode) ? config?.typeCode : (config?.typeCode ? [config?.typeCode] : null);
      return typeCodes ? finalIssueTypes.filter((type) => typeCodes.includes(type.typeCode)) : finalIssueTypes;
    },
    initialData: [] as IIssueType[],
    ...options,
  });
}
