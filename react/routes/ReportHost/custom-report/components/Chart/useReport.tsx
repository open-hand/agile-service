import { useEffect, useState, useCallback } from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { customReportApi } from '@/api';
import { getProjectId } from '@/utils/common';
import { ISearchVO } from '@/common/types';
import { ChartProps } from './index';

export type IChartType = 'line' | 'bar' | 'pie' | 'stackedBar';
export type IChartUnit = 'storyPoints' | 'quantity';

export interface ChartConfig {
  projectId?: undefined
  chartType?: IChartType,
  statisticsType?: IChartUnit,
  analysisField?: string,
  comparedField?: string,
  searchVO: undefined,
  analysisFieldPredefined?: boolean,
  comparedFieldPredefined?: boolean,
}

function useReport(config: ChartConfig, onFinish?: Function): [{}, ChartProps] {
  const projectId = config?.projectId || getProjectId();
  const [data, setData] = useState<any>(null);
  const [loading, setLoading] = useState(false);
  const handleEmpty = useCallback(() => {
    onFinish && setTimeout(onFinish);
  }, [onFinish]);
  const {
    chartType, statisticsType, analysisField, comparedField,
  } = config;
  const loadData = useCallback(async () => {
    if (!statisticsType || !chartType || !analysisField || (chartType === 'stackedBar' && !comparedField)) {
      setLoading(true);
      const res = await customReportApi.project(projectId).getData(config);
      batchedUpdates(() => {
        setData(res);
        setLoading(false);
        onFinish && setTimeout(onFinish);
      });
    } else {
      setData(null);
    }
  }, [analysisField, chartType, comparedField, config, onFinish, projectId, statisticsType]);
  useEffect(() => {
    loadData();
  }, [loadData]);

  const searchProps = {};
  const props: ChartProps = {
    loading,
    data,
    chartType,
    type: statisticsType,
  };
  return [searchProps, props];
}

export default useReport;
