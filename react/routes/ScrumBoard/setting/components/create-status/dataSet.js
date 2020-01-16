import { getProjectId, getOrganizationId } from '@/common/utils';

export default function DataSetFactory() {
  return {
    autoCreate: true,
    transport: {
      create: {
        url: `/agile/v1/projects/${getProjectId()}/issue_status`,
        method: 'post',
        params: {
          applyType: 'agile', 
        },
        transformRequest: (([data]) => JSON.stringify({
          ...data,
          projectId: getProjectId(),
          enable: true,          
        })),
      },
      validate: ({ data: { unique } }) => ({
        url: `agile/v1/projects/${getProjectId()}/status/project_check_name`,
        method: 'GET',
        params: {
          organization_id: getOrganizationId(),
          name: unique[0].name,
        },
        data: null,
        transformResponse: data => !JSON.parse(data).statusExist,
      }),
    },
    fields: [
      {
        name: 'name',
        type: 'string',
        label: '状态名称',
        required: true,
        unique: true,
        
      },
      {
        name: 'categoryCode',
        type: 'string',
        label: '类别',
        required: true, 
        lookupAxiosConfig: () => ({
          url: '/agile/v1/lookup_values/status_category',
          transformResponse: data => (Array.isArray(data) ? data : JSON.parse(data).lookupValues),
        }),
        textField: 'name',
        valueField: 'valueCode',
      },
    ],
  };
}
