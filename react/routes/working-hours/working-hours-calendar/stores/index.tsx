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
}

function getIsRestDay(date: string | Moment, workCalendar: any) {
  return isHoliday({
    sprintSetting: [],
    orgWorkCalendar: workCalendar,
  }, moment(date));
}

export const StoreProvider: React.FC<Context> = inject('AppState')(observer((props: any) => {
  const { children, AppState } = props;
  const calendarDs = useMemo(() => new DataSet(CalendarDataSet()), []);
  const searchDs = useMemo(() => new DataSet(CalendarSearchDataSet({ calendarDs, currentProject: AppState.getCurrentProject })), [AppState.getCurrentProject, calendarDs]);
  const exportDs = useMemo(() => new DataSet(LogExportDataSet({ currentProject: AppState.getCurrentProject })), [AppState.getCurrentProject]);
  const workCalendarMap = useMemo(() => new Map([]), []);
  const [countData, setCountData] = useState<ICountData>({});
  const [loading, setLoading] = useState<boolean>(false);

  const getWorkCalendar = useCallback(() => {
    const minDate = searchDs.current?.get('startTime');
    const maxDate = searchDs.current?.get('endTime');
    if (minDate && maxDate) {
      const yearRange = [moment(minDate).year(), moment(maxDate).year()];
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
  }, [searchDs, workCalendarMap]);

  useEffect(() => {
    getWorkCalendar();
  }, [getWorkCalendar]);

  const getCountData = useCallback((data: IWorkingHoursData) => {
    workingHoursApi.getCount(data).then((res: ICountData) => {
      setCountData(res);
    });
  }, []);

  useEffect(() => {
    const startTime = searchDs.current?.get('startTime');
    const endTime = searchDs.current?.get('endTime');
    if (startTime && endTime) {
      getCountData({
        startTime: formatStartDate(startTime, true) as string,
        endTime: formatEndDate(endTime, true) as string,
        userIds: searchDs.current?.get('userIds'),
        projectIds: searchDs.current?.get('projectIds'),
      });
    }
  }, [getCountData,
    searchDs.current?.get('startTime'),
    searchDs.current?.get('endTime'),
    searchDs.current?.get('userIds'),
    searchDs.current?.get('projectIds'),
  ]);

  const loadData = useCallback(() => {
    // eslint-disable-next-line no-nested-ternary
    const startTime = localPageCacheStore.getItem('workingHours-calendar-startTime') || `${formatStartDate(getIsOrganization() ? moment().subtract(6, 'days') : (
      moment().subtract(6, 'days').isBefore(moment(AppState.getCurrentProject?.creationDate)) ? moment(AppState.getCurrentProject?.creationDate) : moment().subtract(6, 'days')
    ), true)}`;
    const endTime = localPageCacheStore.getItem('workingHours-calendar-endTime') || `${formatEndDate(moment(), true)}`;
    calendarDs.setQueryParameter('startTime', startTime);
    calendarDs.setQueryParameter('endTime', endTime);
    calendarDs.setQueryParameter('userIds', localPageCacheStore.getItem('workingHours-calendar-userIds'));
    calendarDs.setQueryParameter('projectIds', localPageCacheStore.getItem('workingHours-calendar-projectIds'));
    calendarDs.query();
    getCountData({
      startTime,
      endTime,
      userIds: localPageCacheStore.getItem('workingHours-calendar-userIds'),
      projectIds: localPageCacheStore.getItem('workingHours-calendar-projectIds'),
    });
  }, [AppState.getCurrentProject?.creationDate, calendarDs, getCountData]);

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
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
}));
