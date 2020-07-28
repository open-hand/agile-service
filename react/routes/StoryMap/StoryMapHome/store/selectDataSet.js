import { useMemo } from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { getProjectId } from '@/utils/common';
import _ from 'lodash';

const fieldIds = {
  components: 'componentId',
  sprints: 'sprintId',
  prioritys: 'id',
};

const IsCompletedDataSet = {
  data: [
    {
      isCompleted: true,
      name: '已解决',
    },
    {
      isCompleted: false,
      name: '未解决',
    },
  ],
};

const isCompletedDataSet = new DataSet(IsCompletedDataSet);

export default function SelectDataSet(StoryMapStore) {
  return {
    autoCreate: true,
    fields: [
      {
        name: 'isCompleted',
        type: 'boolean',
        label: '解决状态',
        textField: 'name',
        valueField: 'isCompleted',
        options: isCompletedDataSet,
        defaultValue: null,
      },
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
        name: 'prioritys',
        type: 'object',
        label: '优先级',
        lookupAxiosConfig: () => ({
          url: `/agile/v1/projects/${getProjectId()}/priority/list_by_org`,
          method: 'get',
          transformResponse: data => (Array.isArray(data) ? data : (JSON.parse(data) || [])),
        }),
        textField: 'name',
        valueField: 'id',
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
        console.log(name, value);
        if (name === 'isCompleted') {
          StoryMapStore.handleFilterChange(name, value);
        } else {
          StoryMapStore.handleFilterChange(name, value ? value.map(item => item[fieldIds[name]]) : []);
        }
      },
    },
  };
}
