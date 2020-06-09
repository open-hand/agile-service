import { getProjectId, getOrganizationId } from '@/utils/common';
import { Choerodon } from '@choerodon/boot';

export default function DataSetFactory(setCategoryCode) {
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
      // 进行验证 当存在同名状态 将类别自动选择
      validate: ({ data: { unique } }) => ({
        url: `agile/v1/projects/${getProjectId()}/status/project_check_name`,
        method: 'GET',
        params: {
          organization_id: getOrganizationId(),
          name: unique[0].name,
        },
        data: null,
        transformResponse: (res) => {
          const data = JSON.parse(res);
          const { statusExist, type } = data;
          if (statusExist) {
            setCategoryCode(type);
          } else {
            setCategoryCode(null);
          }
          return true;
        },
      }),
    },
    feedback: {
      submitFailed(error) {
        if (error.code === 'error.status.exist') {
          Choerodon.prompt('状态已存在');
        }
      },
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
          transformResponse: data => ((Array.isArray(data) ? data : JSON.parse(data).lookupValues)).filter(status => status.valueCode !== 'prepare'),
        }),
        textField: 'name',
        valueField: 'valueCode',
      },
    ],
  };
}
