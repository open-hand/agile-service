import { set } from 'lodash';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
/* eslint-disable import/no-anonymous-default-export */
interface Props {
  projectId: string,
  organizationId: string
  issueSearchStore: any,
}

const WorkingHoursIssuesDataSet = ({
  projectId, organizationId, issueSearchStore,
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
    read: ({ params, data }) => ({
      url: `/agile/v1/projects/${projectId}/work_hours/issue_work_hours`,
      method: 'post',
      params: {
        ...params,
        containsSubIssue: data.containsSubIssue,
        organizationId,
      },
      transformRequest: () => {
        const search = issueSearchStore?.getCustomFieldFilters() || {};
        set(search, 'searchArgs.tree', true);
        set(search, 'searchArgs.startTime', data.startTime);
        set(search, 'searchArgs.endTime', data.endTime);
        set(search, 'otherArgs.assigneeId', data.assigneeId);
        return JSON.stringify(search);
      },
    }),
  },
  fields: [
    { name: 'summary', type: 'string' as FieldType, label: '概要' },
    { name: 'issueTypeId', type: 'object' as FieldType, label: '工作项类型' },
    { name: 'issueNum', type: 'string' as FieldType, label: '任务编号' },
    { name: 'workTime', type: 'string' as FieldType, label: '工时' },
    { name: 'cumulativeWorkTime', type: 'string' as FieldType, label: '历史累计工时' },
    { name: 'estimateTime', type: 'string' as FieldType, label: '原始预估时间' },
    { name: 'deviationRate', type: 'string' as FieldType, label: '偏差率' },
    { name: 'priorityId', type: 'string' as FieldType, label: '优先级' },
    { name: 'statusId', type: 'object' as FieldType, label: '状态' },
    { name: 'assigneeId', type: 'string' as FieldType, label: '经办人' },
    { name: 'reporterId', type: 'string' as FieldType, label: '报告人' },
    { name: 'createdBy', type: 'object' as FieldType, label: '创建人' },
    { name: 'lastUpdatedBy', type: 'object' as FieldType, label: '更新人' },
    { name: 'label', type: 'string' as FieldType, label: '标签' },
    { name: 'component', type: 'string' as FieldType, label: '模块' },
    { name: 'storyPoints', type: 'string' as FieldType, label: '故事点' },
    { name: 'fixVersion', type: 'string' as FieldType, label: '修复的版本' },
    { name: 'influenceVersion', type: 'string' as FieldType, label: '影响的版本' },
    { name: 'epicId', type: 'string' as FieldType, label: '史诗' },
    { name: 'featureId', type: 'string' as FieldType, label: '特性' },
    { name: 'lastUpdateDate', type: 'string' as FieldType, label: '最后更新时间' },
    { name: 'creationDate', type: 'string' as FieldType, label: '创建时间' },
    { name: 'estimatedStartTime', type: 'string' as FieldType, label: '预计开始时间' },
    { name: 'estimatedEndTime', type: 'string' as FieldType, label: '预计结束时间' },
    { name: 'remainingTime', type: 'string' as FieldType, label: '剩余预估时间' },
    { name: 'spentWorkTime', type: 'string' as FieldType, label: '已耗费时间' },
    { name: 'allEstimateTime', type: 'string' as FieldType, label: '当前预估时间' },
    { name: 'issueSprintVOS', type: 'array' as FieldType, label: '冲刺' },
    { name: 'mainResponsibleId', type: 'object' as FieldType, label: '主要负责人' },
    { name: 'environment', type: 'string' as FieldType, label: '环境' },
    { name: 'tags', type: 'string' as FieldType, label: 'Tag' },
    { name: 'actualStartTime', type: 'string' as FieldType, label: '实际开始时间' },
    { name: 'actualEndTime', type: 'string' as FieldType, label: '实际结束时间' },
  ],
});

export default WorkingHoursIssuesDataSet;
