import { issueTypeApi } from '@/api';
import { IIssueType } from '@/common/types';
import { useQuery, UseQueryOptions } from 'react-query';
import useIsProgram from '../useIsProgram';
import useProjectKey from './useProjectKey';

export interface ProjectIssueTypesConfig {
  projectId?: string
  typeCode?: string
}
export default function useProjectIssueTypes(config?: ProjectIssueTypesConfig, options?: UseQueryOptions<IIssueType[]>) {
  const { isProgram } = useIsProgram();
  const applyType = isProgram ? 'program' : 'agile';
  const key = useProjectKey({ key: 'issueTypes', projectId: config?.projectId });
  return useQuery(key, () => issueTypeApi.loadAllWithStateMachineId(applyType), {
    select: (data) => {
      const issueTypes = (!isProgram ? data.filter((item: IIssueType) => item.typeCode !== 'feature') : data);
      return config?.typeCode ? issueTypes.filter((type) => type.typeCode === config?.typeCode) : issueTypes;
    },
    ...options,
  });
}
