import { useState, useEffect, useCallback } from 'react';
import { reportApi } from '@/api';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { getProjectId } from '@/utils/common';
import { IterationSpeedProps, ISprintSpeed } from './index';
import { IterationSpeedSearchProps, IUnit } from './search';
import { IChartSearchHookAdditionalConfig } from '../types';
import useGetChartSearchDataSet from '../useGetChartSearchDataSet';

export interface IterationSpeedConfig extends IChartSearchHookAdditionalConfig{
  unit?: IUnit
  projectId?: string
}

const useIterationSpeedReport = (config?: IterationSpeedConfig, onFinish?: Function):[IterationSpeedProps, IterationSpeedSearchProps] => {
  const projectId = config?.projectId || getProjectId();
  const [loading, setLoading] = useState<boolean>(false);
  const [unit, setUnit] = useControlledDefaultValue<IUnit>(config?.unit || 'story_point');
  const [data, setData] = useState<ISprintSpeed[]>([]);

  const loadChartData = useCallback(() => {
    setLoading(true);
    reportApi.project(projectId).loadVelocity(unit).then((res: ISprintSpeed[]) => {
      setUnit(unit);
      setData(res);
      setLoading(false);
      onFinish && setTimeout(onFinish);
    });
  }, [projectId, setUnit, unit, onFinish]);

  useEffect(() => {
    loadChartData();
  }, [loadChartData]);

  const props = {
    loading, unit, data,
  };
  const searchDataSet = useGetChartSearchDataSet({
    enabled: config?.openValidate,
    fields: [
      { name: 'unit', label: '单位', required: true },
    ],
    valueChangeDataSetValue: {
      unit,
    },
  });

  const searchProps = {
    unit, setUnit, projectId, searchDataSet,
  };
  return [props, searchProps];
};

export default useIterationSpeedReport;
