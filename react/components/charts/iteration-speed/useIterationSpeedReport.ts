import { useState, useEffect, useCallback } from 'react';
import { reportApi } from '@/api';
import { IterationSpeedProps, ISprintSpeed } from './index';
import { IterationSpeedSearchProps, IUnit } from './search';

const useIterationSpeedReport = (): [IterationSpeedProps, IterationSpeedSearchProps] => {
  const [loading, setLoading] = useState<boolean>(false);
  const [unit, setUnit] = useState<IUnit>('story_point');
  const [data, setData] = useState<ISprintSpeed[]>([]);

  const loadChartData = useCallback(() => {
    setLoading(true);
    reportApi.loadVelocity(unit).then((res: ISprintSpeed[]) => {
      setUnit(unit);
      setData(res);
      setLoading(false);
    });
  }, [unit]);

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
