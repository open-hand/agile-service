import {
  useEffect, useState, useCallback,
} from 'react';
import { EpicBurnDownChartProps, OriginData, ChartData } from '@/components/charts/epic-burnDown';
import { EpicBurnDownSearchProps, IEpic } from '@/components/charts/epic-burnDown/search';
import { reportApi, epicApi } from '@/api';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { getChartDataFromServerData } from './utils';

interface EpicBurnConfig {
  epicId: string
  checked: 'checked' | undefined
}

function useEpicBurnDownReport(config?: EpicBurnConfig): [EpicBurnDownSearchProps, EpicBurnDownChartProps] {
  const [epics, setEpics] = useState<IEpic[]>([]);
  const [epicIsLoading, setEpicIsLoading] = useState<boolean>(false);
  const [checked, setChecked] = useControlledDefaultValue<'checked' | undefined>(config?.checked);
  const [currentEpicId, setCurrentEpicId] = useControlledDefaultValue<string>(config?.epicId || '');
  const [loading, setLoading] = useState<boolean>(false);
  const [data, setData] = useState<OriginData[]>([]);
  const [chartData, setChartData] = useState<ChartData>([[], [], [], [], [], [], [], []]);

  const loadEpic = useCallback(() => {
    setLoading(true);
    setEpicIsLoading(true);
    epicApi.loadEpics().then((epicList:IEpic[]) => {
      setEpicIsLoading(false);
      setEpics(epicList);
      let initEpicId = '';
      if (config?.epicId) {
        initEpicId = config.epicId;
      } else {
        initEpicId = epicList.length ? epicList[0].issueId : '';
      }
      setCurrentEpicId(initEpicId);
    });
  }, [config?.epicId, setCurrentEpicId]);

  useEffect(() => {
    loadEpic();
  }, [loadEpic]);

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
    if (currentEpicId) {
      loadChartData(currentEpicId);
    } else {
      setLoading(false);
    }
  }, [currentEpicId, loadChartData]);

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
