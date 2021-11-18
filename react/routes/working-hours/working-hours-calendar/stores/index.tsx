/* eslint-disable react-hooks/exhaustive-deps */
import React, {
  createContext, useCallback, useContext, useEffect, useMemo, useState,
} from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { inject } from 'mobx-react';
import { observer } from 'mobx-react-lite';
import moment, { Moment } from 'moment';
import { AppStateProps, User } from '@/common/types';
import CalendarSearchDataSet, { formatEndDate, formatStartDate } from './CalendarSearchDataSet';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getIsOrganization } from '@/utils/common';
import LogExportDataSet from '../../working-hours-log/stores/LogExportDataSet';
import { IWorkingHoursData, workCalendarApi, workingHoursApi } from '@/api';
import isHoliday from '@/utils/holiday';
import CalendarDataSet from './CalendarDataSet';

const Store = createContext({} as Context);

export function useCalendarStore() {
  return useContext(Store);
}

export interface ICalendarData {
  userId: string
  userMessageDTO: User
  allEstimateTime: number
  countMap: {
    [key: string]: number
  }
}

export type ICountData = {[date: string]: number };

interface Context {
  searchDs: DataSet,
  exportDs: DataSet,
  AppState: AppStateProps,
  loadData: () => void
  isRestDay: (date: string | Moment) => boolean
  calendarDs: DataSet
  countData: ICountData
  getCountData: (data: IWorkingHoursData) => void
  loading: boolean,
  setLoading: (loading: boolean) => void
  startTime: string,
  endTime: string,
  userIds: undefined | string[],
  projectIds: undefined | string[],
  workGroupIds: undefined | string[],
}

function getIsRestDay(date: string | Moment, workCalendar: any) {
  return isHoliday({
    sprintSetting: [],
    orgWorkCalendar: workCalendar,
  }, moment(date));
}

export const StoreProvider: React.FC<Context> = inject('AppState')(observer((props: any) => {
  const { children, AppState } = props;
  const projectCreationDate = useMemo(() => AppState.getCurrentProject?.creationDate, [AppState.getCurrentProject]);
  const calendarDs = useMemo(() => new DataSet(CalendarDataSet()), []);
  const searchDs = useMemo(() => new DataSet(CalendarSearchDataSet({ projectCreationDate })), [projectCreationDate]);
  const exportDs = useMemo(() => new DataSet(LogExportDataSet({ projectCreationDate })), [projectCreationDate]);
  const workCalendarMap = useMemo(() => new Map([]), []);
  const [countData, setCountData] = useState<ICountData>({});
  const [loading, setLoading] = useState<boolean>(false);
  // eslint-disable-next-line no-nested-ternary
  const startTime = useMemo(() => formatStartDate(searchDs.current?.get('startTime'), true) || localPageCacheStore.getItem('workingHours-calendar-startTime') || (`${formatStartDate(getIsOrganization() ? moment().subtract(6, 'days') : (
    moment().subtract(6, 'days').isBefore(moment(projectCreationDate)) ? moment(projectCreationDate) : moment().subtract(6, 'days')
  ), true)}`), [searchDs.current?.get('startTime'), projectCreationDate]);
  const endTime = useMemo(() => (formatEndDate(searchDs.current?.get('endTime'), true) || localPageCacheStore.getItem('workingHours-calendar-endTime') || `${formatEndDate(moment(), true)}`), [searchDs.current?.get('endTime')]);
  const userIds = useMemo(() => searchDs.current?.get('userIds') || localPageCacheStore.getItem('workingHours-calendar-userIds'), [searchDs.current?.get('userIds')]);
  const projectIds = useMemo(() => searchDs.current?.get('projectIds') || localPageCacheStore.getItem('workingHours-calendar-projectIds'), [searchDs.current?.get('projectIds')]);
  const workGroupIds = useMemo(() => (userIds?.length ? undefined : (searchDs.current?.get('workGroupIds') || localPageCacheStore.getItem('workingHours-calendar-workGroupIds'))), [userIds, searchDs.current?.get('workGroupIds')]);
  const getWorkCalendar = useCallback(() => {
    if (startTime && endTime) {
      const yearRange = [moment(startTime).year(), moment(endTime).year()];
      if (!isNaN(yearRange[0]) && !isNaN(yearRange[1])) {
        if (yearRange[0] === yearRange[1]) {
          workCalendarApi.getWorkSetting(yearRange[0]).then((res: any) => {
            workCalendarMap.set(yearRange[0], res);
          });
        } else {
          Promise.all(yearRange.map((year) => workCalendarApi.getWorkSetting(year))).then((yearCalendars) => {
            yearRange.forEach((year, i) => {
              workCalendarMap.set(year, yearCalendars[i]);
            });
          });
        }
      }
    }
  }, [workCalendarMap, startTime, endTime]);

  useEffect(() => {
    getWorkCalendar();
  }, [getWorkCalendar]);

  const getCountData = useCallback((data: IWorkingHoursData) => {
    workingHoursApi.getCount(data).then((res: ICountData) => {
      setCountData(res);
    });
  }, []);

  const loadData = useCallback(() => {
    calendarDs.setQueryParameter('startTime', startTime);
    calendarDs.setQueryParameter('endTime', endTime);
    calendarDs.setQueryParameter('userIds', userIds);
    calendarDs.setQueryParameter('projectIds', projectIds);
    calendarDs.setQueryParameter('workGroupIds', workGroupIds);
    calendarDs.query();
    getCountData({
      startTime,
      endTime,
      userIds,
      projectIds,
      workGroupIds,
    });
  }, [calendarDs, getCountData, startTime, endTime, userIds, projectIds, workGroupIds]);

  useEffect(() => {
    loadData();
  }, [loadData]);

  const isRestDay = useCallback((date) => getIsRestDay(date, workCalendarMap.get(date.year()) || {}), [workCalendarMap]);
  const value = {
    ...props,
    searchDs,
    exportDs,
    loadData,
    isRestDay,
    calendarDs,
    getCountData,
    countData,
    loading,
    setLoading,
    startTime,
    endTime,
    userIds,
    projectIds,
    workGroupIds,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
}));
