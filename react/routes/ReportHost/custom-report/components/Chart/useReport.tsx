import {
  useEffect, useState, useCallback, useMemo,
} from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { customReportApi, fieldApi } from '@/api';
import { getProjectId } from '@/utils/common';
import { IField, ISearchVO } from '@/common/types';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { getCustomFieldFilters } from '@/components/issue-export/utils';
import { getTransformSystemFilter } from '@/routes/Issue/components/ExportIssue/utils';
import IssueFilterForm, { useIssueFilterForm } from '@/components/issue-filter-form';
import ChooseField, { useChoseField } from '@/components/chose-field';
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
  const handleEmpty = useCallback(() => {
    onFinish && setTimeout(onFinish);
  }, [onFinish]);

  useEffect(() => {
    const getCustomFields = async () => {
      const fields = await fieldApi.getCustomFields();
      setCustomFields(fields);
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
        choseFieldStore.delChosenFields(item.code);
      },
    },
  });

  const search = getCustomFieldFilters(choseFieldStore.getAllChosenField, filterData.dataSet.current!, getTransformSystemFilter);

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const searchVO = useMemo(() => search, [JSON.stringify(search)]);
  console.log(searchVO);
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
        setData(res.dimensionList || []);
        onFinish && setTimeout(onFinish);
      });
    }
  }, [analysisField, analysisFieldPredefined, chartType, comparedField, comparedFieldPredefined, config, onFinish, projectId, searchVO, statisticsType]);
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
    searchVO,
    choseFieldStore,
    choseComponentProps,
    filterComponentProps,
    fields,
    filterData,
  };
  return [searchProps, props];
}

export default useReport;
