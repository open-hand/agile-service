import {
  useState, useEffect, useCallback, useMemo,
} from 'react';
import { rdqamApi } from '@/api';
import moment from 'moment';
import { getProjectId } from '@/utils/common';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { CodeQualityVaryProps, CodeQualityVaryData } from './index';
import { CodeQualityVarySearchProps } from './search';

export interface CodeQualityVaryConfig {
  projectId?: string
  days?: number
}

const useCodeQualityVaryReport = (config?: CodeQualityVaryConfig, onFinish?: Function): [CodeQualityVaryProps, CodeQualityVarySearchProps] => {
  const projectId = config?.projectId || getProjectId();
  const [loading, setLoading] = useState<boolean>(false);
  const [data, setData] = useState<CodeQualityVaryData>({ legend: [], sonarIssueHistoryDetails: [], date: [] });
  const [days, setDays] = useControlledDefaultValue(config?.days ?? 7);
  const { startDate, endDate } = useMemo(() => ({
    startDate: `${moment().subtract(days, 'days').format('YYYY-MM-DD')} 00:00:00`,
    endDate: `${moment().add(1, 'days').format('YYYY-MM-DD')} 00:00:00`,
  }), [days]);
  const loadChartData = useCallback(() => {
    setLoading(true);
    rdqamApi.project(projectId).loadHistory(startDate, endDate).then((res: CodeQualityVaryData) => {
      setData(res);
      setLoading(false);
      onFinish && setTimeout(onFinish);
    });
  }, [projectId, startDate, endDate, onFinish]);

  useEffect(() => {
    loadChartData();
  }, [loadChartData]);

  const props = {
    loading,
    data,
  };
  const searchProps = {
    projectId,
    days,
    setDays,
  };
  return [props, searchProps];
};

export default useCodeQualityVaryReport;
