import {
  useEffect, useState, useCallback, useMemo,
} from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { customReportApi, fieldApi } from '@/api';
import { getProjectId } from '@/utils/common';
import { IField } from '@/common/types';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { getCustomFieldFilters } from '@/components/issue-export/utils';
import { getTransformSystemFilter } from '@/routes/Issue/components/ExportIssue/utils';
import { useIssueFilterForm } from '@/components/issue-filter-form';
import { useChoseField } from '@/components/chose-field';
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
}

function useReport(config: ChartConfig, maxShow = 12, onFinish?: Function): [{}, ChartProps] {
  const projectId = config?.projectId || getProjectId();
  const [data, setData] = useState<IChartData[] | null>(null);
  const [loading, setLoading] = useState(false);
  const [customFields, setCustomFields] = useState<IField[]>([]);
  const [hasGetCustomFields, setHasGetCustomFields] = useState<boolean>(false);

  useEffect(() => {
    const getCustomFields = async () => {
      const fields = await fieldApi.getCustomFields();
      setCustomFields(fields);
      setHasGetCustomFields(true);
    };
    getCustomFields();
  }, []);

  const fields = useMemo(() => [...customFields, ...getSystemFields()], [customFields]);

  const [choseDataProps, choseComponentProps] = useChoseField({
    fields,
  });

  const { store: choseFieldStore } = choseDataProps;

  const [filterData, filterComponentProps] = useIssueFilterForm({
    fields,
    value: choseFieldStore.getAllChosenField,
    events: {
      afterDelete: (item) => {
        choseFieldStore.delChosenFields(item.id || item.code);
      },
    },
  });

  const search = hasGetCustomFields ? getCustomFieldFilters(choseFieldStore.getAllChosenField, filterData.dataSet.current!, getTransformSystemFilter) : undefined;

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const searchVO = useMemo(() => search, [JSON.stringify(search)]);
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
      const res = await customReportApi.project(projectId).getData({ ...config, searchVO });
      batchedUpdates(() => {
        setLoading(false);
        setData((res.dimensionList || []).map((item: IChartData) => ({ ...item, pointList: item.pointList.map((point) => ({ ...point, value: parseFloat(point.value.toString()) })) })));
        onFinish && setTimeout(onFinish);
      });
    }
  }, [analysisField, analysisFieldPredefined, chartType, comparedField, comparedFieldPredefined, config, onFinish, projectId, searchVO, statisticsType]);
  useEffect(() => {
    loadData();
  }, [loadData]);

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const choseFieldStoreMemo = useMemo(() => choseFieldStore, [JSON.stringify(choseFieldStore)]);
  const searchProps = {};
  const props: ChartProps = useMemo(() => ({
    loading,
    data,
    chartType,
    type: statisticsType,
    option: data?.length ? getOptions(chartType as IChartType, statisticsType as IChartUnit, data as IChartData[], maxShow) : undefined,
    searchVO,
    choseFieldStore: choseFieldStoreMemo,
    choseComponentProps,
    filterComponentProps,
    fields,
    filterData,
    hasGetCustomFields,
  }), [chartType, choseComponentProps, choseFieldStoreMemo, data, fields, filterComponentProps, filterData, hasGetCustomFields, loading, maxShow, searchVO, statisticsType]);
  return [searchProps, props];
}

export default useReport;
