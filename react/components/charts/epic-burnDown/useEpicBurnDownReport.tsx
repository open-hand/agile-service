import {
  useEffect, useState, useCallback,
} from 'react';
import { EpicBurnDownChartProps, OriginData, ChartData } from '@/components/charts/epic-burnDown';
import { EpicBurnDownSearchProps, IEpic } from '@/components/charts/epic-burnDown/search';
import { reportApi, epicApi } from '@/api';
import { getChartDataFromServerData } from './utils';

function useEpicBurnDownReport(): [EpicBurnDownSearchProps, EpicBurnDownChartProps] {
  const [epics, setEpics] = useState<IEpic[]>([]);
  const [epicIsLoading, setEpicIsLoading] = useState<boolean>(false);
  const [checked, setChecked] = useState<'checked' | undefined>();
  const [currentEpicId, setCurrentEpicId] = useState<string>('');
  const [loading, setLoading] = useState<boolean>(false);
  const [data, setData] = useState<OriginData[]>([]);
  const [chartData, setChartData] = useState<ChartData>([[], [], [], [], [], [], [], []]);

  const loadChartData = useCallback((id?) => {
    if (id || currentEpicId) {
      setLoading(true);
      reportApi.loadEpicOrVersionBurnDownCoordinate(id || currentEpicId, 'Epic')
        .then((res: OriginData[]) => {
          setData(res);
          setChartData(getChartDataFromServerData(res));
          setLoading(false);
        }).catch(() => {
          setLoading(false);
        });
    }
  }, [currentEpicId]);

  useEffect(() => {
    loadChartData();
  }, [loadChartData]);

  const loadData = useCallback(() => {
    setLoading(true);
    setEpicIsLoading(true);
    epicApi.loadEpics().then((epicList:IEpic[]) => {
      setEpicIsLoading(false);
      setEpics(epicList);
      const firstEpicId = epicList.length && epicList[0].issueId;
      setCurrentEpicId(firstEpicId || '');
      if (firstEpicId) {
        loadChartData(firstEpicId);
      } else {
        setLoading(false);
      }
    });
  }, []);

  useEffect(() => {
    loadData();
  }, [loadData]);

  const searchProps: EpicBurnDownSearchProps = {
    epics,
    epicIsLoading,
    checked,
    currentEpicId,
    setCurrentEpicId,
    setChecked,
  };
  const props: EpicBurnDownChartProps = {
    data,
    chartData,
    loading,
    checked,
  };
  return [searchProps, props];
}

export default useEpicBurnDownReport;
