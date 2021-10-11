import { isNull } from 'lodash';

// 工作项完成趋势-故事
const CompleteStoryDataSet = ({ projectId }) => ({
  paging: false,
  autoQuery: false,
  transport: {
    read: ({ data, params }) => ({
      url: `/agile/v1/projects/${projectId}/team_performance/history_story_point`,
      params: { ...data, ...params },
      method: 'get',
    }),
  },
});

// 工作项完成趋势-任务工时
const CompleteTaskDataSet = ({ projectId }) => ({
  paging: false,
  autoQuery: false,
  transport: {
    read: ({ data, params }) => ({
      url: `/agile/v1/projects/${projectId}/team_performance/history_task_time`,
      params: { ...data, ...params },
      method: 'get',
    }),
  },
});

// 缺陷趋势分析
const BugTendencyDataSet = ({ projectId }) => ({
  paging: false,
  autoQuery: false,
  transport: {
    read: ({ data, params }) => {
      const {
        environment, type, responsibleIds = [],
      } = data;
      return {
        url: `/agile/v1/projects/${projectId}/team_performance/history_bug_count`,
        params: { environment, type, other: responsibleIds.includes('other') },
        data: responsibleIds.filter((responsibleId) => responsibleId !== 'other'),
        method: 'post',
      };
    },
  },
});

// 缺陷排行榜头部操作
const BugRespnsibleDataSet = ({ projectId }) => ({
  autoQuery: false,
  autoCreate: true,
  fields: [
    {
      name: 'responsibleId',
      label: '责任人',
      textField: 'realName',
      valueField: 'id',
      multiple: true,
      lookupAxiosConfig: () => ({
        url: `/agile/v1/projects/${projectId}/team_performance/responsible`,
        method: 'get',
        transformResponse: (res) => {
          if (typeof res === 'string') {
            const data = JSON.parse(res);
            const nextData = data.map((item) => {
              if (isNull(item.id)) {
                return { ...item, id: 'other', realName: '未分配' };
              }
              return item;
            });
            return [...nextData];
          }
          return res;
        },
      }),
    },
  ],
});

export {
  CompleteStoryDataSet, CompleteTaskDataSet, BugTendencyDataSet, BugRespnsibleDataSet,
};
