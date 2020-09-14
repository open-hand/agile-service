import {
  useEffect, useState, useCallback,
} from 'react';
import { axios } from '@choerodon/boot';
import { EpicBurnDownChartProps } from '@/components/charts/epic-burnDown';
import { EpicBurnDownSearchProps, IEpic } from '@/components/charts/epic-burnDown/search';

function useEpicBurnDownReport(): [EpicBurnDownSearchProps, EpicBurnDownChartProps] {
  const [epics, setEpics] = useState<IEpic[]>([]);
  const [epicFinishLoading, setEpicFinishLoading] = useState<boolean>(false);
  const [checked, setChecked] = useState<'checked' | undefined>();
  const [currentEpicId, setCurrentEpicId] = useState<string>('');
  const searchProps: EpicBurnDownSearchProps = {
    epics,
    epicFinishLoading,
    checked,
    currentEpicId,
    setCurrentEpicId,
    setChecked,
  };
  const props: EpicBurnDownChartProps = {
    epics,
    epicFinishLoading,
    checked,
    currentEpicId,
  };
  return [searchProps, props];
}

export default useEpicBurnDownReport;
