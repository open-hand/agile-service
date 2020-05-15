import { getProjectId } from '@/utils/common';

export default function DataSetFactory({ sequence, boardId }) {
  return {
    autoCreate: true,
    transport: {
      create: ({ data }) => ({
        url: `/agile/v1/projects/${getProjectId()}/board_column`,
        method: 'post',
        params: {
          applyType: 'agile', 
          categoryCode: data[0].categoryCode,
        },
        // eslint-disable-next-line no-shadow
        transformRequest: (([data]) => JSON.stringify({
          ...data,
          sequence,
          boardId,
          projectId: getProjectId(),
          enable: true,          
        })),
      }),      
    },
    fields: [
      {
        name: 'name',
        type: 'string',
        label: '列名称',
        required: true,       
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
