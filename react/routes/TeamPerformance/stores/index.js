import React, { createContext, useMemo } from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import { remove } from 'lodash';
import { DimensionDataSet, EfficiencyStoryDataSet, EfficiencyTaskDataSet } from './EfficiencyDataSet';
import {
  BugRankDataSet, BugRankHandleDataSet, QualityRankDataSet, QualityRankHandleDataSet, BugDistributionDataSet,
} from './QualityDataSet';
import {
  CompleteStoryDataSet, CompleteTaskDataSet, BugTendencyDataSet,
} from './TendencyDataSet';

const Store = createContext();
export default Store;

function transformRes({
  res, plan, complete, percent,
}) {
  try {
    const nextRes = JSON.parse(res);
    if (nextRes.length) {
      const response = {
        xData: [],
        plan: [],
        complete: [],
        percent: [],
      };
      const copyNestRes = [...nextRes];
      // 过滤掉未分配&&计划故事点为0或null的数据，没有实际意义
      remove(copyNestRes, (item) => !item.mainResponsibleId && !item[plan]);
      copyNestRes.forEach((item) => {
        response.xData.push(item.realName || '未分配');
        response.plan.push(item[plan] || 0);
        response.complete.push(item[complete] || 0);
        response.percent.push(item[percent] || 0);
      });
      return response;
    }
    return {};
  } catch (err) {
    return {};
  }
}

export const StoreProvider = (props) => {
  const { children } = props;
  const { AppState: { currentMenuType: { id } } } = stores;
  // 进度与效率
  const dimensionDS = useMemo(() => new DataSet(DimensionDataSet()), []);
  const lineDimensionDS = useMemo(() => new DataSet(DimensionDataSet()), []);
  const efficiencyStoryDS = useMemo(() => new DataSet(EfficiencyStoryDataSet({
    projectId: id,
  })), []);
  const lineEfficiencyStoryDS = useMemo(() => new DataSet({
    ...EfficiencyStoryDataSet({ projectId: id }),
    transport: {
      read: () => ({
        url: `/agile/v1/projects/${id}/team_performance/story_point`,
        method: 'get',
        transformResponse: (res) => transformRes({
          res,
          plan: 'storyPoints',
          complete: 'storyPointsComplete',
          percent: 'mainStoryPointsPlanCompleteRate',
        }),
      }),
    },
  }), []);
  const efficiencyTaskDS = useMemo(() => new DataSet(EfficiencyTaskDataSet({
    projectId: id,
  })), []);
  const lineEfficiencyTaskDS = useMemo(() => new DataSet({
    ...EfficiencyTaskDataSet({ projectId: id }),
    transport: {
      read: () => ({
        url: `/agile/v1/projects/${id}/team_performance/task_time`,
        method: 'get',
        transformResponse: (res) => transformRes({
          res,
          plan: 'remainingTime',
          complete: 'remainingTimeComplete',
          percent: 'remainingTimePlanCompleteRate',
        }),
      }),
    },
  }), []);

  // 质量分析
  const bugRankDataSet = useMemo(() => new DataSet(BugRankDataSet({ projectId: id })), []);
  const bugRankHandleDataSet = useMemo(() => new DataSet(BugRankHandleDataSet()), []);
  const qualityRankDataSet = useMemo(() => new DataSet(QualityRankDataSet({ projectId: id })), []);
  const qualityRankHandleDataSet = useMemo(() => new DataSet(QualityRankHandleDataSet({ projectId: id })), []);
  const bugChartHandleDataSet = useMemo(() => new DataSet(BugRankHandleDataSet()), []);
  const bugDistributionDataSet = useMemo(() => new DataSet(BugDistributionDataSet({ projectId: id })), []);
  // 趋势分析
  const questionTendencyHandleDS = useMemo(() => new DataSet(DimensionDataSet()), []);
  const completeTendencyHandleDS = useMemo(() => new DataSet(DimensionDataSet()), []);
  const completeStoryDS = useMemo(() => new DataSet(CompleteStoryDataSet({
    projectId: id,
  })), []);
  const completeTaskDS = useMemo(() => new DataSet(CompleteTaskDataSet({
    projectId: id,
  })), []);
  const bugTendencyChartHandleDS = useMemo(() => new DataSet(BugRankHandleDataSet()), []);
  const bugTendencyDS = useMemo(() => new DataSet(BugTendencyDataSet({
    projectId: id,
  })), []);
  const value = {
    ...props,
    dimensionDS,
    lineDimensionDS,
    efficiencyStoryDS,
    efficiencyTaskDS,
    lineEfficiencyStoryDS,
    lineEfficiencyTaskDS,
    bugRankDataSet,
    bugRankHandleDataSet,
    qualityRankDataSet,
    qualityRankHandleDataSet,
    bugChartHandleDataSet,
    bugDistributionDataSet,
    questionTendencyHandleDS,
    completeTendencyHandleDS,
    completeStoryDS,
    completeTaskDS,
    bugTendencyChartHandleDS,
    bugTendencyDS,
    projectId: id,
    prefixCls: 'c7n-team-performance',
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
};
