/* eslint-disable react-hooks/exhaustive-deps */
import React, {
  createContext, useCallback, useContext, useEffect, useMemo, useState, useRef,
} from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { inject } from 'mobx-react';
import { observer, useComputed } from 'mobx-react-lite';
import moment from 'moment';
import { set } from 'lodash';
import { useCreation } from 'ahooks';
import { AppStateProps, IFoundationHeader } from '@/common/types';
import DateSearchDataSet, { formatEndDate, formatStartDate } from './DateSearchDataSet';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import {
  getIsOrganization, getProjectId, getOrganizationId, getMenuType,
} from '@/utils/common';
import WorkingHoursIssuesDataSet from './WorkingHoursIssuesDataSet';
import AssigneeDataSet from './AssigneeDataSet';
import { useIssueSearchStore } from '@/components/issue-search';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import IssueSearchStore, { ILocalField, IssueSearchStoreProps } from '@/components/issue-search/store';
import { transformFilter } from '@/routes/Issue/stores/utils';
import useIssueTableFields from '@/hooks/data/useIssueTableFields';
import { ListLayoutColumnVO, workingHoursApi } from '@/api';
import ProjectDataSet from './ProjectDataSet';
import { getWorkbenchSystemFields, transformWorkbenchFilter } from '../utils/searchUtils';

const Store = createContext({} as Context);

export function useIssueStore() {
  return useContext(Store);
}

interface Context {
  dateSearchDs: DataSet,
  workingHoursIssuesDs: DataSet,
  AppState: AppStateProps,
  loadData: () => void
  recordDs: DataSet
  loading: boolean,
  setLoading: (loading: boolean) => void
  mode: IMode
  setMode: (mode: IMode) => void
  isProject: string
  issueSearchStore: IssueSearchStore
  myDefaultFilter?: any
  workingHoursAssigneeDs: DataSet
  workingHoursProjectDs: DataSet
  workingHoursProjectAssigneeDs: DataSet
  tableFields: IFoundationHeader[]
  isContain: boolean,
  setIsContain: (isContain: false) => void,
  onCloseDetail: (expandRecordId: string) => void
  totalWorkTime: number
  startTime: string,
  endTime: string,
  onCloseDetailTableQuery: (ds: DataSet) => void
}

export type IMode = 'issue' | 'assignee' | 'project' | 'projectAssignee';
export const StoreProvider: React.FC<Context> = inject('AppState')(observer((props: any) => {
  const { children, AppState, myDefaultFilter } = props;
  const isProject = getMenuType() === 'project';
  const [loading, setLoading] = useState<boolean>(false);
  const [mode, setMode] = useState<IMode>(localPageCacheStore.getItem('workingHours-issue-mode') || (isProject ? 'issue' : 'project'));
  const [isContain, setIsContain] = useState<boolean>(false);
  const [totalWorkTime, setTotalWorkTime] = useState<number>(0);
  const issueSearchStoreProps = useCreation(():IssueSearchStoreProps => (isProject ? ({
    getSystemFields: () => getSystemFields() as ILocalField[],
    transformFilter,
    menuType: 'project',
    defaultSearchVO: localPageCacheStore.getItem('agile.working.hours.issue.search') ?? (myDefaultFilter && myDefaultFilter.filterJson ? JSON.parse(myDefaultFilter.filterJson) : undefined) ?? undefined,
  }) : ({
    getSystemFields: getWorkbenchSystemFields,
    transformFilter: transformWorkbenchFilter,
    menuType: 'workbench',
  })), [isProject]);
  const issueSearchStore = useIssueSearchStore(issueSearchStoreProps);

  const { data: tableFields } = useIssueTableFields({
    extraFields: [{ code: 'workTime', title: '工时' } as IFoundationHeader,
    { code: 'cumulativeWorkTime', title: '历史累计工时' } as IFoundationHeader,
    { code: 'estimateTime', title: '原始预估时间' } as IFoundationHeader,
    { code: 'deviationRate', title: '偏差率' } as IFoundationHeader,
    ],
  });
  useEffect(() => () => { localPageCacheStore.setItem('agile.working.hours.issue.search', issueSearchStore.getCustomFieldFilters()); }, []);

  const workingHoursIssuesDs = useMemo(() => new DataSet(WorkingHoursIssuesDataSet({
    projectId: getProjectId(),
    organizationId: getOrganizationId(),
    issueSearchStore,
  })), []);
  const workingHoursAssigneeDs = useMemo(() => new DataSet(AssigneeDataSet({
    projectId: isProject ? getProjectId() : undefined,
    organizationId: getOrganizationId(),
    issueSearchStore,
  })), []);
  const workingHoursProjectDs = useMemo(() => new DataSet(ProjectDataSet({
    organizationId: getOrganizationId(),
    issueSearchStore,
  })), []);
  const workingHoursProjectAssigneeDs = useMemo(() => new DataSet(ProjectDataSet({
    organizationId: getOrganizationId(),
    issueSearchStore,
  })), []);
  const dataSetMap = useMemo(() => new Map([
    ['issue', workingHoursIssuesDs],
    ['assignee', workingHoursAssigneeDs],
    ['project', workingHoursProjectDs],
    ['projectAssignee', workingHoursProjectAssigneeDs],
  ]), []);
  const dateSearchDs = useMemo(() => new DataSet(DateSearchDataSet({ currentProject: AppState.getCurrentProject })), [AppState.getCurrentProject]);
  // eslint-disable-next-line no-nested-ternary
  const startTime = useMemo(() => (dateSearchDs.current?.get('startTime') && formatStartDate(dateSearchDs.current?.get('startTime'), true)) || localPageCacheStore.getItem('workingHours-issue-startTime') || `${formatStartDate(getIsOrganization() ? moment().subtract(6, 'days') : (
    moment().subtract(6, 'days').isBefore(moment(AppState.getCurrentProject?.creationDate)) ? moment(AppState.getCurrentProject?.creationDate) : moment().subtract(6, 'days')
  ), true)}`, [dateSearchDs.current?.get('startTime')]);
  const endTime = useMemo(() => (dateSearchDs.current?.get('endTime') && formatEndDate(dateSearchDs.current?.get('endTime'), true)) || localPageCacheStore.getItem('workingHours-issue-endTime') || `${formatEndDate(moment(), true)}`, [dateSearchDs.current?.get('endTime')]);

  const search = useComputed(() => issueSearchStore.getCustomFieldFilters(), [issueSearchStore]);

  const getTotalWorkTime = useCallback(async () => {
    set(search, 'searchArgs.startTime', startTime);
    set(search, 'searchArgs.endTime', endTime);
    const res = await workingHoursApi.getTotalWorkTime(search, isContain);
    const totalCount = res.toString().split('.')[1] && res.toString().split('.')[1].length > 1 ? res.toFixed(1) : res;
    setTotalWorkTime(totalCount);
  }, [search, startTime, endTime, isContain]);

  const loadData = useCallback(() => {
    const dataSet = dataSetMap.get(mode) as DataSet;
    if (startTime && endTime) {
      dataSet.setQueryParameter('startTime', startTime);
      dataSet.setQueryParameter('endTime', endTime);
      dataSet.setQueryParameter('containsSubIssue', isContain);
      dataSet.query();
      getTotalWorkTime();
    }
  }, [startTime, endTime, mode, isContain, search]);

  useEffect(() => {
    loadData();
  }, [loadData]);

  const onCloseDetailTableQuery = useCallback(async (ds: DataSet) => {
    const expandIssueIds = new Map([]);
    ds.forEach((record) => {
      // @ts-ignore
      if (record.isExpanded) {
        expandIssueIds.set(record.get('issueId'), true);
      }
    });
    await ds.query(ds.currentPage);
    ds.forEach((record) => {
      if (expandIssueIds.get(record.get('issueId'))) {
        // eslint-disable-next-line no-param-reassign
        record.isExpanded = true;
      }
    });
  }, []);

  const onCloseDetail = useCallback(async (expandRecordId) => {
    const dataSet = dataSetMap.get(mode) as DataSet;
    const expandedRecordsMap = new Map([]);
    if (mode === 'assignee' || mode === 'project' || mode === 'projectAssignee') {
      const mapKey = mode === 'assignee' ? 'userId' : 'projectId';
      dataSet.records.forEach((record) => {
        if (record.getState('recordDs')) {
          expandedRecordsMap.set(record.get(mapKey), record.getState('recordDs'));
        }
      });
      const assignExpandedRecordsMap = new Map([]);

      if (mode === 'projectAssignee' && expandedRecordsMap.size) { // 项目经办人维度，打开的有经办人
        for (const [projectId, assignDs] of expandedRecordsMap) {
          (assignDs as DataSet).records.forEach((record) => {
            if (record.getState('recordDs')) {
              assignExpandedRecordsMap.set(`${projectId}-${record.get('userId')}`, record.getState('recordDs'));
            }
          });
        }
      }
      await dataSet.query(dataSet.currentPage);
      dataSet.records.forEach(async (record) => {
        if (expandedRecordsMap.get(record.get(mapKey).toString())) {
          const recordDs = expandedRecordsMap.get(record.get(mapKey)) as DataSet;
          record.setState('recordDs', recordDs);
          // eslint-disable-next-line no-param-reassign
          record.isExpanded = true;
          if (record.get(mapKey).toString() === expandRecordId.toString()) { // 只刷新打开详情所属record的DataSet当前页
            mode === 'projectAssignee' ? await recordDs.query(recordDs.currentPage) : await onCloseDetailTableQuery(recordDs); // 对于IssueTable,记住层级
            if (mode === 'projectAssignee' && assignExpandedRecordsMap.size) {
              recordDs.records.forEach((assignRecord) => {
                const assignRecordDs = assignExpandedRecordsMap.get(`${expandRecordId}-${assignRecord.get('userId')}`) as DataSet;
                if (assignRecordDs) {
                  assignRecord.setState('recordDs', assignRecordDs);
                  // eslint-disable-next-line no-param-reassign
                  assignRecord.isExpanded = true;
                  onCloseDetailTableQuery(assignRecordDs);
                }
              });
            }
          }
        }
      });
    }
    getTotalWorkTime();
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
    workingHoursProjectDs,
    workingHoursProjectAssigneeDs,
    isProject,
    tableFields,
    startTime,
    endTime,
    isContain,
    setIsContain,
    onCloseDetail,
    totalWorkTime,
    onCloseDetailTableQuery,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
}));
