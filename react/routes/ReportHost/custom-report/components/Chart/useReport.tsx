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
  analysisFieldPredefined?: boolean,
  comparedFieldPredefined?: boolean,
  searchVO?: ISearchVO,
}

function useReport(config: ChartConfig, maxShow = 12, onFinish?: Function): [{}, ChartProps] {
  const projectId = config?.projectId || getProjectId();
  const [data, setData] = useState<IChartData[] | null>(null);
  const [loading, setLoading] = useState(false);

  const {
    chartType, statisticsType, analysisField, analysisFieldPredefined, comparedField, comparedFieldPredefined,
  } = config;
  const loadData = useCallback(async () => {
    if (!statisticsType || !chartType || !analysisField || analysisFieldPredefined === undefined || (chartType === 'stackedBar' && (!comparedField || comparedFieldPredefined === undefined))) {
      batchedUpdates(() => {
        setData(null);
        setLoading(false);
      });
    } else {
      setLoading(true);
      const res = await customReportApi.project(projectId).getData({ ...config });
      batchedUpdates(() => {
        setLoading(false);
        setData((res.dimensionList || []).map((item: IChartData) => ({ ...item, pointList: item.pointList.map((point) => ({ ...point, value: parseFloat(point.value.toString()) })) })));
        onFinish && setTimeout(onFinish);
      });
    }
  }, [analysisField, analysisFieldPredefined, chartType, comparedField, comparedFieldPredefined, config, onFinish, projectId, statisticsType]);
  useEffect(() => {
    loadData();
  }, [loadData]);

  const searchProps = {};
  const props: ChartProps = useMemo(() => ({
    loading,
    data,
    chartType,
    type: statisticsType,
    option: data?.length ? getOptions(chartType as IChartType, statisticsType as IChartUnit, data as IChartData[], maxShow) : undefined,
  }), [chartType, data, loading, maxShow, statisticsType]);
  return [searchProps, props];
}

export default useReport;
