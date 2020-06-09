import { getProjectId } from '@/utils/common';
import _ from 'lodash';

export default function SelectDataSet(StoryMapStore) {
  return {
    autoCreate: true,
    fields: [
      {
        name: 'components',
        type: 'object',
        label: '模块',
        lookupAxiosConfig: () => ({
          url: `/agile/v1/projects/${getProjectId()}/component/query_all`,
          params: {
            size: 0,
            page: 1,
          },
          data: { advancedSearchArgs: {}, searchArgs: {}, content: '' },
          method: 'post',
          transformResponse: data => (Array.isArray(data) ? data : (JSON.parse(data).content || [])),
        }),
        textField: 'name',
        valueField: 'componentId',
      },
      {
        name: 'sprints',
        type: 'object',
        label: '冲刺',
        lookupAxiosConfig: () => ({
          url: `/agile/v1/projects/${getProjectId()}/sprint/names`,
          method: 'post',
          transformResponse: (data) => {
            if (Array.isArray(data)) {
              return data;
            } else {
              const newData = JSON.parse(data) || [];
              const newDataGroupByStatus = _.groupBy(newData, 'statusCode');
              return [...(newDataGroupByStatus.started || []), ...(newDataGroupByStatus.sprint_planning || []), ...(newDataGroupByStatus.closed || [])];
            }
          },
        }),
        textField: 'sprintName',
        valueField: 'sprintId',
      },
    ],
    events: {
      update: ({ name, value }) => {
        StoryMapStore.handleFilterChange(name, value ? value.map(item => (name === 'components' ? item.componentId : item.sprintId)) : []);
      },
    },
  };
}
