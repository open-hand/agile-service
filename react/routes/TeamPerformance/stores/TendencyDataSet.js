// 问题完成趋势-故事
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

// 问题完成趋势-任务工时
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
    read: ({ data, params }) => ({
      url: `/agile/v1/projects/${projectId}/team_performance/history_bug_count`,
      params: { ...data, ...params },
      method: 'get',
    }),
  },
});

export { CompleteStoryDataSet, CompleteTaskDataSet, BugTendencyDataSet };
