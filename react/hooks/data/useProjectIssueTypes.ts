import { useQuery, UseQueryOptions } from 'react-query';
import { issueTypeApi } from '@/api';
import { IIssueType } from '@/common/types';
import useIsProgram from '../useIsProgram';
import useProjectKey from './useProjectKey';
import useIsInProgram from '../useIsInProgram';

export interface ProjectIssueTypesConfig {
  applyType?: 'agile' | 'program'
  projectId?: string
  /** 只返回某一类的工作项类型 */
  typeCode?: string | string[]
  /** 只查询启用的 */
  onlyEnabled?: boolean
  programId?: string | number
  isProgram?: boolean
  isInProgram?: boolean
  menuType?: 'project' | 'org'
}
export default function useProjectIssueTypes(config?: ProjectIssueTypesConfig, options?: UseQueryOptions<IIssueType[]>) {
  const { isProgram } = useIsProgram(); // 这里对项目群判断 对 config?.projectId 传入项目未做判断
  const { isInProgram } = useIsInProgram({ projectId: config?.projectId });
  const key = useProjectKey({ key: ['issueTypes', { onlyEnabled: config?.onlyEnabled ?? true, isProgram: config?.isProgram ?? isProgram }], projectId: config?.projectId });
  return useQuery(key, () => issueTypeApi.loadAllWithStateMachineId('agile', config?.projectId, config?.onlyEnabled ?? true, config?.programId), {
    select: (data) => {
      const issueTypes = (!(config?.isProgram ?? isProgram) ? data.filter((item: IIssueType) => item.typeCode !== 'feature') : data);
      const finalIssueTypes = ((config?.isInProgram ?? isInProgram) || config?.menuType === 'org') ? issueTypes.filter((item) => item.typeCode !== 'issue_epic') : issueTypes;
      // eslint-disable-next-line no-nested-ternary
      const typeCodes = Array.isArray(config?.typeCode) ? config?.typeCode : (config?.typeCode ? [config?.typeCode] : null);
      return typeCodes ? finalIssueTypes.filter((type) => typeCodes.includes(type.typeCode)) : finalIssueTypes;
    },
    initialData: [] as IIssueType[],
    ...options,
  });
}
