import {
  useEffect, useState, useCallback,
} from 'react';
import { VersionBurnDownChartProps, OriginData, ChartData } from '@/components/charts/version-burnDown';
import { VersionBurnDownSearchProps, IVersion } from '@/components/charts/version-burnDown/search';
import { reportApi, versionApi } from '@/api';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { getProjectId } from '@/utils/common';
import { getChartDataFromServerData } from './utils';

export interface VersionBurnConfig {
  versionId?: string
  checked?: 'checked' | undefined
  projectId?: string
}

function useVersionBurnDownReport(config?: VersionBurnConfig): [VersionBurnDownSearchProps, VersionBurnDownChartProps] {
  const projectId = config?.projectId || getProjectId();
  const [versions, setVersions] = useState<IVersion[]>([]);
  const [versionIsLoading, setVersionIsLoading] = useState<boolean>(false);
  const [checked, setChecked] = useControlledDefaultValue<'checked' | undefined>(config?.checked);
  const [currentVersionId, setCurrentVersionId] = useControlledDefaultValue<string>(config?.versionId || '');
  const [loading, setLoading] = useState<boolean>(false);
  const [data, setData] = useState<OriginData[]>([]);
  const [chartData, setChartData] = useState<ChartData>([[], [], [], [], [], [], [], []]);

  const loadVersions = useCallback(() => {
    setVersionIsLoading(true);
    setLoading(true);
    versionApi.project(projectId).loadNamesByStatus(['version_planning', 'released']).then((versionList:IVersion[]) => {
      setVersionIsLoading(false);
      setVersions(versionList);
      let initVersionId = '';
      if (config?.versionId) {
        initVersionId = config.versionId;
      } else {
        initVersionId = versionList.length ? versionList[0].versionId : '';
      }
      setCurrentVersionId(initVersionId);
    });
  }, [config?.versionId, projectId, setCurrentVersionId]);

  useEffect(() => {
    loadVersions();
  }, [loadVersions]);

  const loadChartData = useCallback((id?) => {
    if (id || currentVersionId) {
      setLoading(true);
      reportApi.project(projectId).loadEpicOrVersionBurnDownCoordinate(id || currentVersionId, 'Vpic')
        .then((res: OriginData[]) => {
          setData(res);
          setChartData(getChartDataFromServerData(res));
          setLoading(false);
        }).catch(() => {
          setLoading(false);
        });
    }
  }, [currentVersionId, projectId]);

  useEffect(() => {
    loadChartData();
  }, [loadChartData]);

  const loadData = useCallback(() => {
    setLoading(true);
    if (currentVersionId) {
      loadChartData(currentVersionId);
    } else {
      setLoading(false);
    }
  }, [currentVersionId, loadChartData]);

  useEffect(() => {
    loadData();
  }, [loadData]);

  const searchProps: VersionBurnDownSearchProps = {
    versions,
    versionIsLoading,
    checked,
    currentVersionId,
    projectId,
    setCurrentVersionId,
    setChecked,
  };
  const props: VersionBurnDownChartProps = {
    data,
    chartData,
    loading,
    checked,
  };
  return [searchProps, props];
}

export default useVersionBurnDownReport;
