import { useQuery, UseQueryOptions } from 'react-query';
import { useCreation, usePersistFn, useWhyDidYouUpdate } from 'ahooks';
import { filter, unionBy, uniqBy } from 'lodash';
import { useCallback } from 'react';
import { fieldApi } from '@/api';
import { IFoundationHeader } from '@/common/types';
import useIsInProgram from '../useIsInProgram';
import useProjectKey from './useProjectKey';

export interface IssueTableFieldsConfig {
  /** 隐藏的字段code  可响应 */
  hiddenFieldCodes?: string[]
  projectId?: string
  programId?: string
  /** 项目(project) 会查询项目层及组织层字段 ; 组织(org) 仅有组织层   */
  menuType?: 'project' | 'org' | 'program'
  /** 额外字段  可响应  */
  extraFields?: IFoundationHeader[]
  /**
   * 表头数据来源于
   */
  issueTypeList?: 'agileIssueType' | 'programIssueType' | ''

}
const systemFields = [
  { code: 'summary', title: '概要' },
  { code: 'issueNum', title: '编号' },
  { code: 'priority', title: '优先级' },
  { code: 'status', title: '状态' },
  { code: 'assignee', title: '经办人' },
  { code: 'reporter', title: '报告人' },
  { code: 'createUser', title: '创建人' },
  { code: 'updateUser', title: '更新人' },
  { code: 'label', title: '标签' },
  { code: 'component', title: '模块' },
  { code: 'storyPoints', title: '故事点' },
  { code: 'fixVersion', title: '修复的版本' },
  { code: 'influenceVersion', title: '影响的版本' },
  { code: 'epic', title: '史诗' },
  { code: 'epicSelfName', title: '史诗名称' },
  { code: 'feature', title: '特性' },
  { code: 'lastUpdateDate', title: '最后更新时间' },
  { code: 'creationDate', title: '创建时间' },
  { code: 'estimatedStartTime', title: '预计开始时间' },
  { code: 'estimatedEndTime', title: '预计结束时间' },
  { code: 'actualStartTime', title: '实际开始时间' },
  { code: 'actualEndTime', title: '实际结束时间' },
  { code: 'remainingTime', title: '剩余预估时间' },
  { code: 'spentWorkTime', title: '已耗费时间' },
  { code: 'allEstimateTime', title: '当前预估时间' },
  { code: 'sprint', title: '冲刺' },
  { code: 'mainResponsibleUser', title: '主要负责人' },
  { code: 'environmentName', title: '环境' },
  { code: 'tags', title: 'Tag' },
  { code: 'participants', title: '参与人' },
  { code: 'estimateTime', title: '原始预估时间' },
] as IFoundationHeader[];
export default function useIssueTableFields(config?: IssueTableFieldsConfig, options?: UseQueryOptions<IFoundationHeader[]>) {
  const key = useProjectKey({ key: ['IssueTableFields'], projectId: `${config?.menuType}-${config?.projectId}` });
  const { isInProgram, loading } = useIsInProgram({ projectId: config?.projectId, menuType: config?.menuType });
  const select: UseQueryOptions<IFoundationHeader[]>['select'] = useCallback((res) => {
    const loadedFields = unionBy(config?.extraFields || [], systemFields, 'code').concat(res);
    return (config?.hiddenFieldCodes ? loadedFields.filter((field) => !config.hiddenFieldCodes?.includes(field.code)) : loadedFields);
  }, [config?.extraFields, config?.hiddenFieldCodes]);

  const { data, ...others } = useQuery(key, () => fieldApi.program(config?.programId).project(config?.projectId)
    .getFoundationHeader(config?.issueTypeList).then((res: any = []) => (config?.menuType === 'org' ? filter(res, (item) => String(item.code).indexOf('org_') === 0) : res)), {
    enabled: !loading,
    initialData: systemFields,
    select,
    ...options,
  });
  const fieldData = useCreation(() => (!isInProgram ? data?.filter((f) => f.code !== 'feature') : data), [isInProgram, data]);
  return {
    ...others,
    data: fieldData,
  };
}
