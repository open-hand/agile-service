import {
  useEffect, useState, useCallback, useMemo,
} from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { DataSet } from 'choerodon-ui/pro';
import moment, { Moment } from 'moment';
import { reportApi, boardApi } from '@/api';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { getProjectId } from '@/utils/common';
import { AccumulationSearchProps } from './search';
import { AccumulationChartProps } from '.';
import { IAccumulationData } from './utils';
import { IChartSearchHookAdditionalConfig } from '../types.';

export interface AccumulationConfig extends IChartSearchHookAdditionalConfig {
  boardId?: string
  defaultLoading?: boolean
  quickFilterIds?: string[]
  range?: [Moment, Moment]
  projectId?: string
}

function useAccumulationReport(config?: AccumulationConfig, onFinish?: Function): [AccumulationSearchProps, AccumulationChartProps, Function] {
  const projectId = config?.projectId || getProjectId();
  const defaultDate = useMemo<[Moment, Moment]>(() => [moment().subtract(2, 'months'), moment()], []);
  const [quickFilterIds, setQuickFilterIds] = useControlledDefaultValue<string[]>(config?.quickFilterIds || []);
  const [data, setData] = useState<IAccumulationData[]>([]);
  const [boardId, setBoardId] = useState<string>(config?.boardId || '');
  const [columnIds, setColumnIds] = useState<string[]>([]);
  const [loading, setLoading] = useState(() => !!config?.defaultLoading);
  const [range, setRange] = useControlledDefaultValue<[Moment, Moment]>(config?.range || defaultDate);
  const loadData = useCallback(async () => {
    if (boardId !== '' && columnIds.length > 0) {
      setLoading(true);
      const [startDate, endDate] = range;
      const burnDownData = await reportApi.project(projectId).loadCumulativeData({
        columnIds,
        endDate: `${endDate.format('YYYY-MM-DD')} 23:59:59`,
        quickFilterIds,
        startDate: startDate.format('YYYY-MM-DD 00:00:00'),
        boardId,
      });
      batchedUpdates(() => {
        setData(burnDownData);
        setLoading(false);
        onFinish && setTimeout(onFinish, 1000);
      });
    }
  }, [boardId, columnIds, onFinish, projectId, quickFilterIds, range]);
  useEffect(() => {
    loadData();
  }, [loadData]);

  const handleBoardChange = useCallback(async (newBoardId: string) => {
    const boardData = await boardApi.project(projectId).load(newBoardId, {});
    // @ts-ignore
    const newColumnIds = boardData.columnsData.columns.map((column) => column.columnId);
    batchedUpdates(() => {
      setBoardId(newBoardId);
      setColumnIds(newColumnIds);
    });
  }, [projectId]);
  useEffect(() => {
    if (config?.boardId) {
      handleBoardChange(config.boardId);
    }
  }, [config?.boardId, handleBoardChange]);
  const searchDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      { name: 'range', label: '范围', required: true },
      { name: 'board', required: true },
    ],
  }), []);
  useEffect(() => {
    boardId && searchDataSet.current?.set('board', boardId);
  }, [boardId, searchDataSet]);
  useEffect(() => {
    range !== searchDataSet.current?.get('range') && searchDataSet.current?.set('range', range);
  }, [range, searchDataSet]);
  const searchProps: AccumulationSearchProps = {
    range,
    onRangeChange: (value) => {
      setRange(value);
    },
    boardId,
    onBoardChange: handleBoardChange,
    quickFilterIds,
    onQuickSearchChange: (value) => {
      setQuickFilterIds(value);
    },
    projectId,
    searchDataSet: config?.openValidate ? searchDataSet : undefined,
  };
  const props: AccumulationChartProps = {
    loading,
    data,
  };
  return [searchProps, props, loadData];
}

export default useAccumulationReport;
