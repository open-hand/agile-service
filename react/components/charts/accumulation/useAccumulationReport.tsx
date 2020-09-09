import { useEffect, useState, useCallback } from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import moment, { Moment } from 'moment';
import { reportApi, boardApi } from '@/api';
import { AccumulationSearchProps } from './search';
import { AccumulationChartProps } from '.';
import { IAccumulationData } from './utils';

interface AccumulationConfig {

}

function useAccumulationReport(config?: AccumulationConfig): [AccumulationSearchProps, AccumulationChartProps] {
  const [quickFilterIds, setQuickFilterIds] = useState<string[]>([]);
  const [data, setData] = useState<IAccumulationData[]>([]);
  const [boardId, setBoardId] = useState<string>('');
  const [columnIds, setColumnIds] = useState<string[]>([]);
  const [loading, setLoading] = useState(false);
  const [range, setRange] = useState<[Moment, Moment]>([moment().subtract(2, 'months'), moment()]);
  const loadData = useCallback(async () => {
    if (boardId !== '') {
      setLoading(true);
      const [startDate, endDate] = range;
      const burnDownData = await reportApi.loadCumulativeData({
        columnIds,
        endDate: `${endDate.format('YYYY-MM-DD')} 23:59:59`,
        quickFilterIds,
        startDate: startDate.format('YYYY-MM-DD 00:00:00'),
        boardId,
      });
      batchedUpdates(() => {
        setData(burnDownData);
        setLoading(false);
      });
    }
  }, [boardId, columnIds, quickFilterIds, range]);
  useEffect(() => {
    loadData();
  }, [loadData]);

  const handleBoardChange = async (newBoardId: string) => {
    const boardData = await boardApi.load(newBoardId, {});
    // @ts-ignore
    const newColumnIds = boardData.columnsData.columns.map((column) => column.columnId);
    batchedUpdates(() => {
      setBoardId(newBoardId);
      setColumnIds(newColumnIds);
    });
  };
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
  };
  const props: AccumulationChartProps = {
    loading,
    data,
  };
  return [searchProps, props];
}

export default useAccumulationReport;
