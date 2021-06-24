import {
  useEffect, useState, useCallback, useMemo,
} from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { customReportApi } from '@/api';
import { getProjectId } from '@/utils/common';
import { ISearchVO } from '@/common/types';
import { ChartProps } from './index';
import getOptions, { IChartData, IChartType, IChartUnit } from './utils';

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

function useReport(config: ChartConfig, maxShow = 12, onFinish?: Function): [{}, ChartProps] {
  const projectId = config?.projectId || getProjectId();
  const [data, setData] = useState<IChartData[] | null>(null);
  const [loading, setLoading] = useState(false);
  const handleEmpty = useCallback(() => {
    onFinish && setTimeout(onFinish);
  }, [onFinish]);
  const {
    chartType, statisticsType, analysisField, comparedField,
  } = config;
  const loadData = useCallback(async () => {
    if (!statisticsType || !chartType || !analysisField || (chartType === 'stackedBar' && !comparedField)) {
      batchedUpdates(() => {
        setData(null);
        setLoading(false);
      });
    } else {
      setLoading(true);
      const res = await customReportApi.project(projectId).getData(config);
      batchedUpdates(() => {
        setLoading(false);
        setData(res.dimensionList || []);
        onFinish && setTimeout(onFinish);
      });
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
    option: data?.length ? getOptions(chartType as IChartType, statisticsType as IChartUnit, data as IChartData[], maxShow) : undefined,
  };
  return [searchProps, props];
}

export default useReport;
