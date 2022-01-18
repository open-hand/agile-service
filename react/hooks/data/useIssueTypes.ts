import { castArray, includes } from 'lodash';
import { useQuery, UseQueryOptions } from 'react-query';
import { usePersistFn } from 'ahooks';
import { useCallback } from 'react';
import { issueTypeApi } from '@/api';
import { IIssueType } from '@/common/types';
import { getIsOrganization, getApplyType } from '@/utils/common';
import useKey from './useKey';
import useIsInProgram from '../useIsInProgram';

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
type IUseIssueTypesData = IIssueType[] | { list: IIssueType[], [key: string]: any };
type IUseIssueTypesQueryOptions = UseQueryOptions<IUseIssueTypesData, unknown, IIssueType[]>;
export default function useIssueTypes(config?: IssueTypeConfig, options?: IUseIssueTypesQueryOptions) {
  const isOrganization = getIsOrganization();
  const { isShowFeature } = useIsInProgram();
  const applyType = config?.applyType ?? getApplyType(true); // 待修 改为==》getApplyType(true)
  let key = config?.hasTemplate ? 'orgHasTemplateIssueTypes' : 'orgIssueTypes';
  key = isOrganization ? key : 'projectIssueTypes';
  const queryKey = useKey({ key: [key, { onlyEnabled: config?.onlyEnabled }], id: config?.id });

  const select: IUseIssueTypesQueryOptions['select'] = useCallback((res) => {
    let issueTypes = [];
    if (isOrganization) {
      issueTypes = ((Array.isArray(res) ? res : res.list) || []).filter((item: IIssueType) => item.typeCode !== 'backlog' && !includes(config?.excludeTypes || [], item.typeCode));
    } else {
      issueTypes = (isShowFeature ? res.filter((item: IIssueType) => item.typeCode !== 'feature' && !includes(config?.excludeTypes || [], item.typeCode)) : res);
    }
    const typeCodes = castArray(config?.typeCode).filter(Boolean);
    return typeCodes.length ? issueTypes.filter((type: IIssueType) => typeCodes.includes(type.typeCode)) : issueTypes;
  }, [config?.excludeTypes, config?.typeCode, isOrganization, isShowFeature]);
  return useQuery<IUseIssueTypesData, unknown, IIssueType[]>(queryKey, () => {
    if (!isOrganization) {
      return issueTypeApi.loadAllWithStateMachineId(applyType, config?.id, config?.onlyEnabled, config?.programId);
    }
    return config?.hasTemplate ? issueTypeApi.orghasTemplateList() : issueTypeApi.orgLoad({ params: { page: 0, size: 0 }, data: {} });
  }, {
    select,
    initialData: [] as IIssueType[],
    ...options,
  });
}
