/* eslint-disable react-hooks/exhaustive-deps */
import React, {
  createContext, useCallback, useContext, useEffect, useMemo, useState,
} from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { inject } from 'mobx-react';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import { AppStateProps, User } from '@/common/types';
import DateSearchDataSet, { formatEndDate, formatStartDate } from './DateSearchDataSet';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getIsOrganization, getProjectId, getOrganizationId } from '@/utils/common';
import WorkingHoursIssuesDataSet from './WorkingHoursIssuesDataSet';

const Store = createContext({} as Context);

export function useIssueStore() {
  return useContext(Store);
}

interface Context {
  dateSearchDs: DataSet,
  workingHoursIssuesDs: DataSet,
  AppState: AppStateProps,
  loadData: () => void
  issueDs: DataSet
  loading: boolean,
  setLoading: (loading: boolean) => void
  mode: IMode
  setMode: (mode: IMode) => void
  isProject: string
}

export type IMode = 'issue' | 'assignee' | 'project' | 'projectAssignee';
export const StoreProvider: React.FC<Context> = inject('AppState')(observer((props: any) => {
  const { children, AppState } = props;
  // @ts-ignore
  const workingHoursIssuesDs = useMemo(() => new DataSet(WorkingHoursIssuesDataSet({
    projectId: getProjectId(),
    organizationId: getOrganizationId(),
  })), []);
  const dateSearchDs = useMemo(() => new DataSet(DateSearchDataSet({ currentProject: AppState.getCurrentProject })), [AppState.getCurrentProject]);
  const [loading, setLoading] = useState<boolean>(false);
  const [mode, setMode] = useState<IMode>(getProjectId() ? 'issue' : 'project');

  const loadData = useCallback(() => {
    // eslint-disable-next-line no-nested-ternary
    const startTime = localPageCacheStore.getItem('workingHours-issue-startTime') || `${formatStartDate(getIsOrganization() ? moment().subtract(6, 'days') : (
      moment().subtract(6, 'days').isBefore(moment(AppState.getCurrentProject?.creationDate)) ? moment(AppState.getCurrentProject?.creationDate) : moment().subtract(6, 'days')
    ), true)}`;
    const endTime = localPageCacheStore.getItem('workingHours-issue-endTime') || `${formatEndDate(moment(), true)}`;
    // issueDs.setQueryParameter('startTime', startTime);
    // issueDs.setQueryParameter('endTime', endTime);
    // issueDs.query();
  }, [AppState.getCurrentProject?.creationDate]);

  useEffect(() => {
    loadData();
  }, [loadData]);

  const value = {
    ...props,
    workingHoursIssuesDs,
    dateSearchDs,
    loadData,
    loading,
    setLoading,
    mode,
    setMode,
    isProject: getProjectId(),
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
}));
