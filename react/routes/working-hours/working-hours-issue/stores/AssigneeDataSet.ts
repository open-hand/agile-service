import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { cloneDeep, set } from 'lodash';

interface props {
  projectId?: string,
  organizationId: string
  issueSearchStore: any,
}

const AssigneeDataSet = ({
  projectId, organizationId, issueSearchStore,
}: props) : DataSetProps => ({
  autoQuery: false,
  paging: true,
  cacheSelection: false,
  transport: {
    read: ({ params, data }) => ({
      url: projectId ? `/agile/v1/projects/${projectId}/work_hours/assignee_work_hours` : `/agile/v1/organizations/${organizationId}/work_hours/assignee_work_hours`,
      method: 'post',
      params: {
        ...params,
        organizationId,
      },
      transformRequest: () => {
        const search = issueSearchStore?.getCustomFieldFilters(true) || {};
        set(search, 'searchArgs.startTime', data.startTime);
        set(search, 'searchArgs.endTime', data.endTime);
        set(search, 'searchArgs.projectIds', data.projectIds);
        return JSON.stringify(search);
      },
    }),
  },
  fields: [
    { name: 'userId', type: 'string' as FieldType, label: '名称' },
    { name: 'workTime', type: 'string' as FieldType, label: '工时' },
    { name: 'cumulativeWorkTime', type: 'string' as FieldType, label: '历史累计工时' },
    { name: 'estimateTime', type: 'string' as FieldType, label: '原始预估时间' },
    { name: 'deviationRate', type: 'string' as FieldType, label: '偏差率' },
  ],
});

export default AssigneeDataSet;
