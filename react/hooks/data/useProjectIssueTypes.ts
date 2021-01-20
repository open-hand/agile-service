import { issueTypeApi } from '@/api';
import { IIssueType } from '@/common/types';
import { useQuery, UseQueryOptions } from 'react-query';
import useIsProgram from '../useIsProgram';
import useProjectKey from './useProjectKey';

export interface ProjectIssueTypesConfig {
  projectId?: string
  /** 只返回某一类的问题类型 */
  typeCode?: string | string[]
}
export default function useProjectIssueTypes(config?: ProjectIssueTypesConfig, options?: UseQueryOptions<IIssueType[]>) {
  const { isProgram } = useIsProgram();
  const applyType = isProgram ? 'program' : 'agile';
  const key = useProjectKey({ key: 'issueTypes', projectId: config?.projectId });
  return useQuery(key, () => issueTypeApi.loadAllWithStateMachineId(applyType), {
    select: (data) => {
      const issueTypes = (!isProgram ? data.filter((item: IIssueType) => item.typeCode !== 'feature') : data);
      // eslint-disable-next-line no-nested-ternary
      const typeCodes = Array.isArray(config?.typeCode) ? config?.typeCode : (config?.typeCode ? [config?.typeCode] : null);
      return typeCodes ? issueTypes.filter((type) => typeCodes.includes(type.typeCode)) : issueTypes;
    },
    placeholderData: [] as IIssueType[],
    ...options,
  });
}
