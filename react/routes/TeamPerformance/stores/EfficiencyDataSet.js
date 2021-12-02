import { remove, orderBy } from 'lodash';
import { DataSet } from 'choerodon-ui/pro';

// 选择维度
const DimensionDataSet = () => ({
  autoQuery: false,
  autoCreate: true,
  fields: [
    {
      name: 'tab',
      type: 'string',
      defaultValue: 'STORY',
      options: new DataSet({
        selection: 'single',
        data: [
          { meaning: '故事点', value: 'STORY', code: 'agile.common.stroy.point' },
          { meaning: '任务工时', value: 'TASK', code: 'agile.common.issue.work.time' },
        ],
      }),
    },
    {
      name: 'dimension',
      label: '选择维度',
      type: 'string',
      defaultValue: 'PLAN',
      options: new DataSet({
        selection: 'single',
        data: [
          { meaning: '计划', value: 'PLAN' },
          { meaning: '实际', value: 'COMPLETE' },
        ],
      }),
    },
  ],
});

/**
 * 转换成拼图需要的数据
 * @param {object} res 请求到的数据
 * @param {string} plan 计划字段
 * @param {string} complete 实际字段
 * @param {string} planRate 计划百分比字段
 * @param {string} completeRate 实际百分比字段
 */
const transformRes = ({
  res, plan, complete, planRate, completeRate,
}) => {
  try {
    const nextRes = JSON.parse(res);
    if (nextRes.length) {
      const response = {
        plan: [],
        complete: [],
        colors: ['#9665E2', '#F0657D', '#FAD352', '#FF9915', '#45A3FC', '#5365EA', '#47CBCA', '#59CB79', '#F953BA', '#D3D3D3'],
      };
      const copyNestRes = [...nextRes];
      // 过滤掉未分配&&计划故事点为0或null的数据，没有实际意义
      remove(copyNestRes, (item) => !item.mainResponsibleId && !item[plan]);
      if (copyNestRes.length > 10) {
        for (let i = 10; i < copyNestRes.length; i += 1) {
          // eslint-disable-next-line
          response.colors.push(`#${(`00000${((Math.random() * 16777215 + 0.5) >> 0).toString(16)}`).slice(-6)}`);
        }
      }
      copyNestRes.forEach(({ name, realName, ...rest }) => {
        response.plan.push({
          name,
          realName,
          value: rest[plan] || 0,
          percent: rest[planRate] || 0,
        });
        response.complete.push({
          name,
          realName,
          value: rest[complete] || 0,
          percent: rest[completeRate] || 0,
        });
      });
      const sortPlanData = response.plan.find((item) => !!item.value) ? orderBy(response.plan, ['percent'], ['desc']) : [];
      const sortCompleteData = response.complete.find((item) => !!item.value) ? orderBy(response.complete, ['percent'], ['desc']) : [];
      return {
        ...response,
        plan: sortPlanData,
        complete: sortCompleteData,
      };
    }
    return {};
  } catch (err) {
    return {};
  }
};

// 当前进行冲刺故事点统计
const EfficiencyStoryDataSet = ({ projectId }) => ({
  paging: false,
  autoQuery: false,
  transport: {
    read: () => ({
      url: `/agile/v1/projects/${projectId}/team_performance/story_point`,
      method: 'get',
      transformResponse: (res) => (
        transformRes({
          res,
          plan: 'storyPoints',
          complete: 'storyPointsComplete',
          planRate: 'mainStoryPointsRate',
          completeRate: 'mainStoryPointsCompleteRate',
        })),
    }),
  },
});

// 当前进行冲刺任务工时统计
const EfficiencyTaskDataSet = ({ projectId }) => ({
  paging: false,
  autoQuery: false,
  transport: {
    read: () => ({
      url: `/agile/v1/projects/${projectId}/team_performance/task_time`,
      method: 'get',
      transformResponse: (res) => (
        transformRes({
          res,
          plan: 'remainingTime',
          complete: 'remainingTimeComplete',
          planRate: 'remainingTimeRate',
          completeRate: 'remainingTimeCompleteRate',
        })
      ),
    }),
  },
});

export { DimensionDataSet, EfficiencyStoryDataSet, EfficiencyTaskDataSet };
