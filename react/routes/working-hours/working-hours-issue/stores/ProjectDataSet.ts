import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { set } from 'lodash';

interface props {
  organizationId: string
  issueSearchStore: any,
  searchDTO: any
}

const ProjectDataSet = ({
  organizationId, issueSearchStore, searchDTO,
}: props) : DataSetProps => ({
  autoQuery: false,
  paging: true,
  cacheSelection: false,
  transport: {
    read: ({ params }) => ({
      url: `/agile/v1/organizations/${organizationId}/work_hours/project_work_hours`,
      method: 'post',
      params: {
        ...params,
      },
      transformRequest: () => {
        const search = searchDTO || issueSearchStore?.getCustomFieldFilters() || {};
        set(search, 'searchArgs.tree', true);
        return JSON.stringify(search);
      },
    }),
  },
  fields: [
    { name: 'projectId', type: 'string' as FieldType, label: '名称' },
    { name: 'workTime', type: 'string' as FieldType, label: '工时' },
    { name: 'cumulativeWorkTime', type: 'string' as FieldType, label: '历史累计工时' },
    { name: 'estimateTime', type: 'string' as FieldType, label: '原始预估时间' },
    { name: 'deviationRate', type: 'string' as FieldType, label: '偏差率' },
  ],
});

export default ProjectDataSet;
