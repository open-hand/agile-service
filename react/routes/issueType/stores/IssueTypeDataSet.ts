import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { getOrganizationId, getIsOrganization } from '@/utils/common';
import DataSet, { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';

import { issueTypeApiConfig } from '@/api';

const sourceDataSet = new DataSet({
  fields: [
    {
      name: 'code',
      type: 'string' as FieldType,
    },
    {
      name: 'label',
      type: 'string' as FieldType,
    },
  ],
  data: [
    {
      code: 'system',
      label: '系统',
    },
    {
      code: 'organization',
      label: '组织',
    },
    ...(getIsOrganization() ? [] : [
      {
        code: 'project',
        label: '项目',
      },
    ]),
  ],
});

const statusDataSet = new DataSet({
  fields: [
    {
      name: 'enabled',
      type: 'boolean' as FieldType,
    },
    {
      name: 'label',
      type: 'string' as FieldType,
    },
  ],
  data: [
    {
      enabled: true,
      label: '启用',
    },
    {
      enabled: false,
      label: '停用',
    },
  ],
});

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
    {
      name: 'source',
      type: 'string' as FieldType,
      label: '来源',
      textField: 'label',
      valueField: 'code',
      options: sourceDataSet,
    },
    ...(getIsOrganization() ? [] : [
      {
        name: 'status',
        type: 'string' as FieldType,
        label: '状态',
        textField: 'label',
        valueField: 'enabled',
        options: statusDataSet,
      },
    ]),
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
    {
      label: '是否允许引用',
      name: 'referenced',
      type: 'boolean' as FieldType,
    },
  ],
  transport: {
    read: ({ params, data }: { params: { page: number, size: number}, data: any}) => {
      if (getIsOrganization()) {
        return issueTypeApiConfig.orgLoad({ params, data });
      }
      return issueTypeApiConfig.load({ params, data });
    },
  },
});
export default IssueTypeDataSet;
