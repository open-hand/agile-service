import { set } from 'lodash';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
/* eslint-disable import/no-anonymous-default-export */
interface Props {
  projectId: string,
  organizationId: string
  issueSearchStore: any,
  searchDTO: any
}

const WorkingHoursIssuesDataSet = ({
  projectId, organizationId, issueSearchStore, searchDTO,
}: Props): DataSetProps => ({
  primaryKey: 'issueId',
  autoQuery: false,
  modifiedCheck: false,
  parentField: 'parentId',
  expandField: 'expand',
  idField: 'issueId',
  paging: 'server',
  cacheSelection: false,
  transport: {
    read: ({ params }) => ({
      url: `/agile/v1/projects/${projectId}/issues/include_sub`,
      method: 'post',
      params: {
        ...params,
        organizationId,
      },
      transformRequest: () => {
        const search = searchDTO || issueSearchStore?.getCustomFieldFilters() || {};
        set(search, 'searchArgs.tree', true);
        return JSON.stringify(search);
      },
    }),
  },
  fields: [
    { name: 'summary', type: 'string' as FieldType, label: '概要' },
    { name: 'issueTypeId', type: 'object' as FieldType, label: '工作项类型' },
    { name: 'issueNum', type: 'string' as FieldType, label: '任务编号' },
    { name: 'workTime', type: 'string' as FieldType, label: '工时' },
    { name: 'historyWorkTime', type: 'string' as FieldType, label: '历史累计工时' },
    { name: 'estimatedWorkTime', type: 'string' as FieldType, label: '预估总工时' },
    { name: 'rate', type: 'string' as FieldType, label: '偏差率' },
    { name: 'priorityId', type: 'string' as FieldType, label: '优先级' },
    { name: 'statusId', type: 'object' as FieldType, label: '状态' },
    { name: 'assigneeId', type: 'string' as FieldType, label: '经办人' },
    { name: 'reporterId', type: 'string' as FieldType, label: '报告人' },
    { name: 'createUser', type: 'object' as FieldType, label: '创建人' },
    { name: 'updateUser', type: 'object' as FieldType, label: '更新人' },
    { name: 'label', type: 'string' as FieldType, label: '标签' },
    { name: 'component', type: 'string' as FieldType, label: '模块' },
    { name: 'storyPoints', type: 'string' as FieldType, label: '故事点' },
    { name: 'fixVersion', type: 'string' as FieldType, label: '修复的版本' },
    { name: 'influenceVersion', type: 'string' as FieldType, label: '影响的版本' },
    { name: 'epic', type: 'string' as FieldType, label: '史诗' },
    { name: 'feature', type: 'string' as FieldType, label: '特性' },
    { name: 'lastUpdateDate', type: 'string' as FieldType, label: '最后更新时间' },
    { name: 'creationDate', type: 'string' as FieldType, label: '创建时间' },
    { name: 'estimatedStartTime', type: 'string' as FieldType, label: '预计开始时间' },
    { name: 'estimatedEndTime', type: 'string' as FieldType, label: '预计结束时间' },
    { name: 'remainingTime', type: 'string' as FieldType, label: '剩余预估时间' },
    { name: 'spentWorkTime', type: 'string' as FieldType, label: '已耗费时间' },
    { name: 'allEstimateTime', type: 'string' as FieldType, label: '当前预估时间' },
    { name: 'issueSprintVOS', type: 'array' as FieldType, label: '冲刺' },
    { name: 'mainResponsibleUser', type: 'object' as FieldType, label: '主要负责人' },
    { name: 'environmentName', type: 'string' as FieldType, label: '环境' },
    { name: 'tags', type: 'string' as FieldType, label: 'Tag' },
  ],
});

export default WorkingHoursIssuesDataSet;
