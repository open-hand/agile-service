import {
  useEffect, useState, useCallback,
} from 'react';
import { VersionBurnDownChartProps, OriginData, ChartData } from '@/components/charts/version-burnDown';
import { VersionBurnDownSearchProps, IVersion } from '@/components/charts/version-burnDown/search';
import { reportApi, epicApi, versionApi } from '@/api';
import { getChartDataFromServerData } from './utils';

function useVersionBurnDownReport(): [VersionBurnDownSearchProps, VersionBurnDownChartProps] {
  const [versions, setVersions] = useState<IVersion[]>([]);
  const [versionIsLoading, setVersionIsLoading] = useState<boolean>(false);
  const [checked, setChecked] = useState<'checked' | undefined>();
  const [currentVersionId, setCurrentVersionId] = useState<string>('');
  const [loading, setLoading] = useState<boolean>(false);
  const [data, setData] = useState<OriginData[]>([]);
  const [chartData, setChartData] = useState<ChartData>([[], [], [], [], [], [], [], []]);

  const loadChartData = useCallback((id?) => {
    if (id || currentVersionId) {
      setLoading(true);
      reportApi.loadEpicOrVersionBurnDownCoordinate(id || currentVersionId, 'Vpic')
        .then((res: OriginData[]) => {
          setData(res);
          setChartData(getChartDataFromServerData(res));
          setLoading(false);
        }).catch(() => {
          setLoading(false);
        });
    }
  }, [currentVersionId]);

  useEffect(() => {
    loadChartData();
  }, [loadChartData]);

  const loadData = useCallback(() => {
    setLoading(true);
    setVersionIsLoading(true);
    versionApi.loadNamesByStatus(['version_planning', 'released']).then((versionList:IVersion[]) => {
      setVersionIsLoading(false);
      setVersions(versionList);
      const firstVerion = versionList.length && versionList[0].versionId;
      setCurrentVersionId(firstVerion || '');
      if (firstVerion) {
        loadChartData(firstVerion);
      } else {
        setLoading(false);
      }
    });
  }, []);

  useEffect(() => {
    loadData();
  }, [loadData]);

  const searchProps: VersionBurnDownSearchProps = {
    versions,
    versionIsLoading,
    checked,
    currentVersionId,
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
