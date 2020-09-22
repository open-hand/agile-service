import { useState, useEffect, useCallback } from 'react';
import { reportApi, epicApi } from '@/api';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { getProjectId } from '@/utils/common';
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

export interface EpicReportConfig {
  unit?: IUnit,
  epicId?: string
  projectId?: string
}

const useEpicReport = (config?: EpicReportConfig): [EpicReportProps, EpicReportSearchProps] => {
  const projectId = config?.projectId || getProjectId();
  const [loading, setLoading] = useState<boolean>(false);
  const [unit, setUnit] = useControlledDefaultValue<IUnit>(config?.unit || 'story_point');
  const [epics, setEpics] = useState<IEpic[]>([]);
  const [epicId, setEpicId] = useControlledDefaultValue<string>(config?.epicId || '');
  const [data, setData] = useState<IEpicReportChart[]>([]);
  const [tableData, setTableData] = useState<IEpicReportTable[]>([]);

  const loadEpics = useCallback(() => {
    setLoading(true);
    epicApi.project(projectId).loadEpics()
      .then((res: IEpic[]) => {
        setLoading(false);
        setEpics(res);
        let initEpicId = '';
        if (config?.epicId) {
          initEpicId = config.epicId;
        } else {
          initEpicId = res.length ? res[0].issueId : '';
        }
        setEpicId(initEpicId);
      });
  }, [config?.epicId, projectId, setEpicId]);

  const loadTableData = useCallback(() => {
    if (epicId) {
      setLoading(true);
      reportApi.project(projectId).loadIssuesForEpic(epicId).then((res: IEpicReportTable[]) => {
        setTableData(res);
        setLoading(false);
      });
    }
  }, [epicId, projectId]);

  const loadData = useCallback(() => {
    if (epicId) {
      setLoading(true);
      reportApi.project(projectId).loadEpicChart(epicId, unit).then((res: IOriginChartData[]) => {
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
  }, [epicId, projectId, unit]);

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
    unit, setUnit, epics, epicId, setEpicId, projectId,
  };
  return [props, searchProps];
};

export default useEpicReport;
