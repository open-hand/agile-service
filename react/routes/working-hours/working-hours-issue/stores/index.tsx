/* eslint-disable react-hooks/exhaustive-deps */
import React, {
  createContext, useCallback, useContext, useEffect, useMemo, useState,
} from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { inject } from 'mobx-react';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import { AppStateProps, IFoundationHeader, User } from '@/common/types';
import DateSearchDataSet, { formatEndDate, formatStartDate } from './DateSearchDataSet';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getIsOrganization, getProjectId, getOrganizationId } from '@/utils/common';
import WorkingHoursIssuesDataSet from './WorkingHoursIssuesDataSet';
import AssigneeDataSet from './AssigneeDataSet';
import { useIssueSearchStore } from '@/components/issue-search';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import IssueSearchStore, { ILocalField } from '@/components/issue-search/store';
import { transformFilter } from '@/routes/Issue/stores/utils';
import useIssueTableFields from '@/hooks/data/useIssueTableFields';

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
  issueSearchStore: IssueSearchStore
  myDefaultFilter?: any
  workingHoursAssigneeDs: DataSet
  tableFields: IFoundationHeader
  isContain: boolean,
  setIsContain: (isContain: false) => void,
  onCloseDetail: () => void
}

export type IMode = 'issue' | 'assignee' | 'project' | 'projectAssignee';
export const StoreProvider: React.FC<Context> = inject('AppState')(observer((props: any) => {
  const { children, AppState, myDefaultFilter } = props;
  const [loading, setLoading] = useState<boolean>(false);
  const [mode, setMode] = useState<IMode>(getProjectId() ? 'issue' : 'project');
  const [isContain, setIsContain] = useState<boolean>(false);
  const issueSearchStore = useIssueSearchStore({
    getSystemFields: () => getSystemFields() as ILocalField[],
    transformFilter,
    defaultSearchVO: localPageCacheStore.getItem('agile.working.hours.issue.search') ?? (myDefaultFilter && myDefaultFilter.filterJson ? JSON.parse(myDefaultFilter.filterJson) : undefined) ?? undefined,
  });

  const { data: tableFields } = useIssueTableFields({
    extraFields: [{ code: 'workTime', title: '工时' } as IFoundationHeader,
      { code: 'historyWorkTime', title: '历史累计工时' } as IFoundationHeader,
      { code: 'estimatedWorkTime', title: '预估总工时' } as IFoundationHeader,
      { code: 'rate', title: '偏差率' } as IFoundationHeader,
    ],
  });
  useEffect(() => () => { localPageCacheStore.setItem('agile.working.hours.issue.search', issueSearchStore.getCustomFieldFilters()); }, []);

  // @ts-ignore
  const workingHoursIssuesDs = useMemo(() => new DataSet(WorkingHoursIssuesDataSet({
    projectId: getProjectId(),
    organizationId: getOrganizationId(),
  })), []);
  const workingHoursAssigneeDs = useMemo(() => new DataSet(AssigneeDataSet({ projectId: getProjectId() })), []);
  const dateSearchDs = useMemo(() => new DataSet(DateSearchDataSet({ currentProject: AppState.getCurrentProject })), [AppState.getCurrentProject]);

  const loadData = useCallback(() => {
    console.log(mode, isContain);
    // eslint-disable-next-line no-nested-ternary
    const startTime = (dateSearchDs.current?.get('startTime') && formatStartDate(dateSearchDs.current?.get('startTime'))) || localPageCacheStore.getItem('workingHours-issue-startTime') || `${formatStartDate(getIsOrganization() ? moment().subtract(6, 'days') : (
      moment().subtract(6, 'days').isBefore(moment(AppState.getCurrentProject?.creationDate)) ? moment(AppState.getCurrentProject?.creationDate) : moment().subtract(6, 'days')
    ), true)}`;
    const endTime = (dateSearchDs.current?.get('endTime') && formatEndDate(dateSearchDs.current?.get('endTime'))) || localPageCacheStore.getItem('workingHours-issue-endTime') || `${formatEndDate(moment(), true)}`;
    if (mode === 'issue') {
      workingHoursIssuesDs.setQueryParameter('startTime', startTime);
      workingHoursIssuesDs.setQueryParameter('endTime', endTime);
      workingHoursIssuesDs.query();
    }
    if (mode === 'assignee') {
      workingHoursAssigneeDs.setQueryParameter('startTime', startTime);
      workingHoursAssigneeDs.setQueryParameter('endTime', endTime);
      workingHoursAssigneeDs.query();
    }
  }, [dateSearchDs.current?.get('startTime'), dateSearchDs.current?.get('endTime'), mode, isContain]);

  useEffect(() => {
    loadData();
  }, [loadData]);

  const onCloseDetail = useCallback(() => {
    if (mode === 'assignee') {
      workingHoursAssigneeDs.records.forEach((record) => {
        console.log(record.isExpanded);
      });
      workingHoursAssigneeDs.query(workingHoursAssigneeDs.currentPage);
    }
    // 加载总计登记工时
  }, [mode]);

  const value = {
    ...props,
    workingHoursIssuesDs,
    dateSearchDs,
    loadData,
    loading,
    setLoading,
    mode,
    setMode,
    issueSearchStore,
    workingHoursAssigneeDs,
    isProject: getProjectId(),
    tableFields,
    isContain,
    setIsContain,
    onCloseDetail,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
}));
