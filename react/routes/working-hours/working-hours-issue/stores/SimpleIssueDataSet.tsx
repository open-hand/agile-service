import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { set } from 'lodash';

interface props {
  organizationId: string
  issueSearchStore: any
}

const SimpleIssueDataSet = ({
  organizationId, issueSearchStore,
}: props) : DataSetProps => ({
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
      url: `/agile/v1/organizations/${organizationId}/work_hours/issue_work_hours`,
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
        set(search, 'searchArgs.projectIds', data.projectIds);
        set(search, 'otherArgs.assigneeId', data.assigneeId);
        return JSON.stringify(search);
      },
    }),
  },
  fields: [
    { name: 'summary', type: 'string' as FieldType, label: '概要' },
    { name: 'issueNum', type: 'string' as FieldType, label: '编号' },
    { name: 'statusId', type: 'object' as FieldType, label: '状态' },
    { name: 'assigneeId', type: 'string' as FieldType, label: '经办人' },
    { name: 'projectId', type: 'string' as FieldType, label: '所属项目' },
    { name: 'workTime', type: 'string' as FieldType, label: '工时' },
    { name: 'cumulativeWorkTime', type: 'string' as FieldType, label: '历史累计工时' },
    { name: 'estimateTime', type: 'string' as FieldType, label: '原始预估时间' },
    { name: 'deviationRate', type: 'string' as FieldType, label: '偏差率' },
  ],
});

export default SimpleIssueDataSet;
