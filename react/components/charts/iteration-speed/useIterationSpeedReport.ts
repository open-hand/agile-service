import { useState, useEffect, useCallback } from 'react';
import { reportApi } from '@/api';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { IterationSpeedProps, ISprintSpeed } from './index';
import { IterationSpeedSearchProps, IUnit } from './search';

interface IterationSpeedConfig {
  unit: IUnit
}

const useIterationSpeedReport = (config?: IterationSpeedConfig):[IterationSpeedProps, IterationSpeedSearchProps] => {
  const [loading, setLoading] = useState<boolean>(false);
  const [unit, setUnit] = useControlledDefaultValue<IUnit>(config?.unit || 'story_point');
  const [data, setData] = useState<ISprintSpeed[]>([]);

  const loadChartData = useCallback(() => {
    setLoading(true);
    reportApi.loadVelocity(unit).then((res: ISprintSpeed[]) => {
      setUnit(unit);
      setData(res);
      setLoading(false);
    });
  }, [setUnit, unit]);

  useEffect(() => {
    loadChartData();
  }, [loadChartData]);

  const props = {
    loading, unit, data,
  };

  const searchProps = {
    unit, setUnit,
  };
  return [props, searchProps];
};

export default useIterationSpeedReport;
