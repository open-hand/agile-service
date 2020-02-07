import { findIndex } from 'lodash';

export default ({
  type, id,
}) => ({
  autoQuery: true,
  paging: false,
  transport: {
    read: ({ data: p, dataSet }) => ({
      url: `/base/v1/${type}s/${id}/users`,
      method: 'get',
      transformResponse: (response) => {
        try {
          const data = JSON.parse(response);
          if (data && data.list) {
            const oldIndex = findIndex(dataSet.toData(), item => item.id === p.userId);
            const index = findIndex(data.list, item => item.id === p.userId);
            if (index === -1 && oldIndex !== -1) {
              data.list.unshift(dataSet.get(oldIndex).toData());
            }
            return data.list;
          } else {
            return data;
          }
        } catch (error) {
          return response;
        }
      },
    }),
  },
});
