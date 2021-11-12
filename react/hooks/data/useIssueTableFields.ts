import { useQuery, UseQueryOptions } from 'react-query';
import { useCreation } from 'ahooks';
import { fieldApi } from '@/api';
import { IFoundationHeader } from '@/common/types';
import useIsInProgram from '../useIsInProgram';
import useProjectKey from './useProjectKey';

export interface IssueTableFieldsConfig {
  hiddenFieldCodes?: string[]
  projectId?: string
  menuType?: 'project' | 'org'
  extraFields?: IFoundationHeader[]
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
  const key = useProjectKey({ key: ['IssueTableFields'], projectId: config?.projectId });
  const { isInProgram, loading } = useIsInProgram({ projectId: config?.projectId, menuType: config?.menuType });
  const { data, ...others } = useQuery(key, () => fieldApi.project(config?.projectId).getFoundationHeader(), {
    enabled: !loading,
    initialData: [...systemFields, ...(config?.extraFields || [])],
    select: (res) => (config?.hiddenFieldCodes ? systemFields.concat(config?.extraFields || []).concat(res).filter((field) => !config.hiddenFieldCodes?.includes(field.code)) : systemFields.concat(config?.extraFields || []).concat(res)),
    ...options,
  });
  const fieldData = useCreation(() => (!isInProgram ? data?.filter((f) => f.code !== 'feature') : data), [isInProgram, data]);
  return {
    ...others,
    data: fieldData,
  };
}
