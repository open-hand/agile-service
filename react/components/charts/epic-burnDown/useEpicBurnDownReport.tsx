import {
  useEffect, useState, useCallback,
} from 'react';
import { EpicBurnDownChartProps, OriginData, ChartData } from '@/components/charts/epic-burnDown';
import { EpicBurnDownSearchProps, IEpic } from '@/components/charts/epic-burnDown/search';
import { reportApi, epicApi } from '@/api';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { getProjectId } from '@/utils/common';
import { getChartDataFromServerData } from './utils';
import { IChartSearchHookAdditionalConfig } from '../types.';
import useGetChartSearchDataSet from '../useGetChartSearchDataSet';

export interface EpicBurnConfig extends IChartSearchHookAdditionalConfig {
  epicId?: string
  checked?: 'checked' | undefined
  projectId?: string
}

function useEpicBurnDownReport(config?: EpicBurnConfig, onFinish?: Function): [EpicBurnDownSearchProps, EpicBurnDownChartProps] {
  const projectId = config?.projectId || getProjectId();
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
    epicApi.project(projectId).loadEpics().then((epicList:IEpic[]) => {
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
  }, [config?.epicId, projectId, setCurrentEpicId]);

  useEffect(() => {
    loadEpic();
  }, [loadEpic]);

  const loadChartData = useCallback((id?) => {
    if (id || currentEpicId) {
      setLoading(true);
      reportApi.project(projectId).loadEpicOrVersionBurnDownCoordinate(id || currentEpicId, 'Epic')
        .then((res: OriginData[]) => {
          setData(res);
          setChartData(getChartDataFromServerData(res));
          setLoading(false);
          onFinish && setTimeout(onFinish);
        }).catch(() => {
          setLoading(false);
        });
    }
  }, [currentEpicId, projectId, onFinish]);
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
  const searchDataSet = useGetChartSearchDataSet({
    enabled: config?.openValidate,
    fields: [
      { name: 'epic', label: '史诗', required: true },
    ],
    valueChangeDataSetValue: {
      spic: currentEpicId,
    },
  });
  const searchProps: EpicBurnDownSearchProps = {
    epics,
    epicIsLoading,
    checked,
    currentEpicId,
    projectId,
    setCurrentEpicId,
    setChecked,
    searchDataSet,
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
