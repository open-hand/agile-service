import { useState, useEffect, useCallback } from 'react';
import { reportApi, epicApi } from '@/api';
import { EpicReportProps, IEpicReportChart, IEpicReportTable } from './index';
import { EpicReportSearchProps, IEpic } from './search';
import { IUnit } from '../iteration-speed/search';

interface IOriginChartData {
  allRemainTimes: number
  allStoryPoints: number
  completedRemainTimes: number
  completedStoryPoints: number
  groupDay: string
  issueCompletedCount: number
  issueCount: number
  unEstimateIssueCount: number
}

const useEpicReport = (): [EpicReportProps, EpicReportSearchProps] => {
  const [loading, setLoading] = useState<boolean>(false);
  const [unit, setUnit] = useState<IUnit>('story_point');
  const [epics, setEpics] = useState<IEpic[]>([]);
  const [epicId, setEpicId] = useState<string>('');
  const [data, setData] = useState<IEpicReportChart[]>([]);
  const [tableData, setTableData] = useState<IEpicReportTable[]>([]);

  const loadEpics = useCallback(() => {
    setLoading(true);
    epicApi.loadEpics()
      .then((res: IEpic[]) => {
        setLoading(false);
        setEpics(res);
        setEpicId(res.length ? res[0].issueId : '');
      });
  }, []);

  const loadTableData = useCallback(() => {
    if (epicId) {
      setLoading(true);
      reportApi.loadIssuesForEpic(epicId).then((res: IEpicReportTable[]) => {
        setTableData(res);
        setLoading(false);
      });
    }
  }, [epicId]);

  const loadData = useCallback(() => {
    if (epicId) {
      setLoading(true);
      reportApi.loadEpicChart(epicId, unit).then((res: IOriginChartData[]) => {
        const chartData = res.map((item) => ({
          ...item,
          allRemainTimes: item.allRemainTimes || 0,
          allStoryPoints: item.allStoryPoints || 0,
          completedRemainTimes: item.completedRemainTimes || 0,
          completedStoryPoints: item.completedStoryPoints || 0,
          issueCompletedCount: item.issueCompletedCount || 0,
          issueCount: item.issueCount || 0,
          unEstimateIssueCount: item.unEstimateIssueCount || 0,
        }));
        setData(chartData);
        setLoading(false);
      });
    }
  }, [unit, epicId]);

  useEffect(() => {
    loadEpics();
  }, [loadEpics]);

  useEffect(() => {
    loadData();
  }, [loadData]);

  useEffect(() => {
    loadTableData();
  }, [loadTableData]);

  const props: EpicReportProps = {
    loading, data, tableData, unit, epicId, epics,
  };
  const searchProps: EpicReportSearchProps = {
    unit, setUnit, epics, epicId, setEpicId,
  };
  return [props, searchProps];
};

export default useEpicReport;
