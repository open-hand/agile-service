import { useQuery, UseQueryOptions } from 'react-query';
import { useCallback } from 'react';
import { issueTypeApi } from '@/api';
import { IIssueType } from '@/common/types';
import useIsProgram from '../useIsProgram';
import useProjectKey from './useProjectKey';
import useIsInProgram from '../useIsInProgram';
import { getApplyType } from '@/utils/common';

export interface ProjectIssueTypesConfig {
  applyType?: 'agile' | 'program' | 'waterfall' | 'risk'
  projectId?: string
  /** 只返回某一类的工作项类型 */
  typeCode?: string | string[]
  /** 只查询启用的 */
  onlyEnabled?: boolean
  programId?: string | number
  isProgram?: boolean
  isInProgram?: boolean
  isShowFeature?: boolean
  menuType?: 'project' | 'org'
}
export default function useProjectIssueTypes(config?: ProjectIssueTypesConfig, options?: UseQueryOptions<IIssueType[]>) {
  const hookIsProgram = useIsProgram(); // 这里对项目群判断 对 config?.projectId 传入项目未做判断
  const isProgram = config?.isProgram ?? hookIsProgram.isProgram;
  const { isShowFeature } = useIsInProgram({ projectId: config?.projectId });
  const applyType = config?.applyType ?? getApplyType(true);
  const key = useProjectKey({ key: ['issueTypes', { onlyEnabled: config?.onlyEnabled ?? true, applyType, typeCode: config?.typeCode }], projectId: config?.projectId });
  const select: UseQueryOptions<IIssueType[]>['select'] = useCallback((data: any[]) => {
    const issueTypes = (!isProgram ? data.filter((item: IIssueType) => item.typeCode !== 'feature') : data);
    const isFilterEpic = config?.applyType !== 'program' && ((config?.isShowFeature ?? isShowFeature) || config?.menuType === 'org');
    const finalIssueTypes = isFilterEpic ? issueTypes.filter((item) => item.typeCode !== 'issue_epic') : issueTypes;
    // eslint-disable-next-line no-nested-ternary
    const typeCodes = Array.isArray(config?.typeCode) ? config?.typeCode : (config?.typeCode ? [config?.typeCode] : null);
    return typeCodes ? finalIssueTypes.filter((type: any) => typeCodes.includes(type.typeCode)) : finalIssueTypes;
  }, [config?.applyType, config?.isShowFeature, config?.menuType, config?.typeCode, isProgram, isShowFeature]);
  return useQuery(key, () => issueTypeApi.loadAllWithStateMachineId(applyType, config?.projectId, config?.onlyEnabled ?? true, config?.programId), {
    select,
    initialData: [] as IIssueType[],
    ...options,
  });
}
