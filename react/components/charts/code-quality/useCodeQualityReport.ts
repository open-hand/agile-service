import { useState, useEffect, useCallback } from 'react';
import { rdqamApi } from '@/api';
import { getProjectId } from '@/utils/common';
import { CodeQualityProps, CodeQualityData } from './index';
import { CodeQualityPropsSearchProps } from './search';

export interface CodeQualityConfig {
  projectId?: string
}

const useCodeQualityReport = (config?: CodeQualityConfig, onFinish?: Function):[CodeQualityProps, CodeQualityPropsSearchProps] => {
  const projectId = config?.projectId || getProjectId();
  const [loading, setLoading] = useState<boolean>(false);
  const [data, setData] = useState<CodeQualityData>({ legend: [], series: [] });

  const loadChartData = useCallback(() => {
    setLoading(true);
    rdqamApi.project(projectId).load().then((res: CodeQualityData) => {
      setData(res);
      setLoading(false);
      onFinish && setTimeout(onFinish);
    });
  }, [projectId, onFinish]);

  useEffect(() => {
    loadChartData();
  }, [loadChartData]);

  const props = {
    loading,
    data,
  };
  const searchProps = {
    projectId,
  };
  return [props, searchProps];
};

export default useCodeQualityReport;
