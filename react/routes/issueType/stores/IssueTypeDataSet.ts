import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { getOrganizationId } from '@/utils/common';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';

const IssueTypeDataSet = (
): DataSetProps => ({
  selection: false,
  paging: true,
  autoQuery: true,
  queryFields: [
    {
      name: 'name',
      type: 'string' as FieldType,
      label: '名称',
    },
  ],
  fields: [
    {
      name: 'name',
      type: 'string' as FieldType,
      label: '名称',
    },
    {
      name: 'action',
    },
    {
      name: 'description',
      type: 'string' as FieldType,
      label: '描述',
    },
    {
      name: 'usage',
      type: 'string' as FieldType,
      label: '使用情况',
    },
    {
      label: '来源',
      name: 'source',
      type: 'string' as FieldType,
    },
    {
      label: '状态',
      name: 'enabled',
      type: 'boolean' as FieldType,
    },
  ],
  transport: {
    read: () => ({
      url: `/agile/v1/organizations/${getOrganizationId()}/issue_type/list`,
      method: 'post',
    }),
  },
});
export default IssueTypeDataSet;
