import { DataSet } from 'choerodon-ui/pro';

// 缺陷排行榜头部操作
const BugRankHandleDataSet = () => ({
  autoQuery: false,
  autoCreate: true,
  fields: [
    {
      name: 'type',
      type: 'string',
      defaultValue: 'dev',
      options: new DataSet({
        selection: 'single',
        data: [
          { meaning: '责任人', code: 'agile.systemField.mainResponsible', value: 'dev' },
          { meaning: '创建人', code: 'agile.common.creator', value: 'test' },
        ],
      }),
    },
    {
      name: 'environment',
      label: '环境',
      type: 'string',
      defaultValue: 'other',
      textField: 'name',
      valueField: 'valueCode',
      lookupAxiosConfig: () => ({
        url: '/agile/v1/lookup_values/environment',
        method: 'get',
        transformResponse: (data) => (Array.isArray(data) ? data : JSON.parse(data).lookupValues),
      }),
    },
  ],
});

// 缺陷排行榜
const BugRankDataSet = ({ projectId }) => ({
  paging: false,
  autoQuery: false,
  transport: {
    read: ({ data, params }) => ({
      url: `/agile/v1/projects/${projectId}/team_performance/bug_rank`,
      params: { ...data, ...params, size: 999 },
      method: 'get',
      transformResponse: (res) => {
        try {
          const nextRes = JSON.parse(res);
          if (nextRes.content.length) {
            let nextData = nextRes.content.filter((item) => !!item.responsibleId);
            nextData = nextData.map((item) => ({ ...item, realName: item.realName || '未分配', name: item.name || '未分配' }));
            return nextData;
          }
          return [];
        } catch (err) {
          return [];
        }
      },
    }),
  },
});

// 代码质量排行榜头部操作
const QualityRankHandleDataSet = ({ projectId }) => ({
  autoQuery: false,
  autoCreate: true,
  fields: [
    {
      name: 'appServiceId',
      label: '选择应用服务',
      textField: 'name',
      valueField: 'id',
      lookupAxiosConfig: () => ({
        url: `/devops/v1/projects/${projectId}/app_service/list_by_active`,
        method: 'get',
      }),
    },
  ],
});

// 代码质量排行榜
const QualityRankDataSet = ({ projectId }) => ({
  paging: false,
  autoQuery: false,
  transport: {
    read: ({ data, params }) => {
      const { appServiceId } = data;
      return {
        url: `/rdqam/v1/projects/${projectId}/team_performance/${appServiceId}/author_issue`,
        params: { ...data, ...params, size: 999 },
        method: 'get',
        transformResponse: (res) => {
          try {
            const nextRes = JSON.parse(res);
            if (nextRes.content.length) {
              let nextData = nextRes.content;
              nextData = nextData.map((item) => {
                const { realName, loginName } = item;
                let name;
                if (realName && loginName) {
                  name = `${realName}(${loginName})`;
                } else if (realName) {
                  name = realName;
                } else if (loginName) {
                  name = loginName;
                } else {
                  name = '未分配';
                }
                return { ...item, realName: item.realName || '未分配', name };
              });
              return nextData;
            }
            return [];
          } catch (err) {
            return [];
          }
        },
      };
    },
  },
});

// 缺陷分布
const BugDistributionDataSet = ({ projectId }) => ({
  paging: false,
  autoQuery: false,
  transport: {
    read: ({ data, params }) => ({
      url: `/agile/v1/projects/${projectId}/team_performance/bug_count`,
      params: { ...data, ...params },
      method: 'get',
      transformResponse: (res) => {
        try {
          const nextRes = JSON.parse(res);
          let nextData = nextRes.filter((item) => !!item.responsibleId);
          nextData = nextData.map((item) => ({ ...item, realName: item.realName || '未分配' }));
          return nextData;
        } catch (err) {
          return [];
        }
      },
    }),
  },
});

export {
  BugRankDataSet,
  BugRankHandleDataSet,
  QualityRankDataSet,
  QualityRankHandleDataSet,
  BugDistributionDataSet,
};
