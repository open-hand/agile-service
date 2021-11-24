import React, {
  createContext, useCallback, useContext, useEffect, useMemo, useState,
} from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { inject } from 'mobx-react';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import LogDataSet from './LogDataSet';
import { AppStateProps } from '@/common/types';
import LogSearchDataSet from './LogSearchDataSet';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getIsOrganization } from '@/utils/common';
import LogExportDataSet from './LogExportDataSet';
import { formatEndDate, formatStartDate } from '../../utils';

const Store = createContext({} as Context);

export function useLogStore() {
  return useContext(Store);
}

interface Context {
  logDs: DataSet,
  logSearchDs: DataSet,
  exportDs: DataSet,
  loadData: () => void
  AppState: AppStateProps,
}

export const StoreProvider: React.FC<Context> = inject('AppState')(observer((props: any) => {
  const { children, AppState } = props;
  const logDs = useMemo(() => new DataSet(LogDataSet()), []);
  const projectCreationDate = useMemo(() => AppState.getCurrentProject?.creationDate, [AppState.getCurrentProject]);
  const logSearchDs = useMemo(() => new DataSet(LogSearchDataSet({ logDs, projectCreationDate })), [logDs, projectCreationDate]);
  const exportDs = useMemo(() => new DataSet(LogExportDataSet({ projectCreationDate })), [projectCreationDate]);
  const loadData = useCallback(() => {
    // eslint-disable-next-line no-nested-ternary
    logDs.setQueryParameter('startTime', localPageCacheStore.getItem('workingHours-log-startTime') || `${formatStartDate(getIsOrganization() ? moment().subtract(6, 'days') : (
      moment().subtract(6, 'days').isBefore(moment(AppState.getCurrentProject?.creationDate)) ? moment(AppState.getCurrentProject?.creationDate) : moment().subtract(6, 'days')
    ), true)}`);
    logDs.setQueryParameter('endTime', localPageCacheStore.getItem('workingHours-log-endTime') || `${formatEndDate(moment(), true)}`);
    logDs.setQueryParameter('userIds', localPageCacheStore.getItem('workingHours-log-userIds'));
    logDs.setQueryParameter('projectIds', localPageCacheStore.getItem('workingHours-log-projectIds'));
    logDs.query();
  }, [AppState.getCurrentProject?.creationDate, logDs]);

  useEffect(() => {
    loadData();
  }, [loadData, logDs]);

  const value = {
    ...props,
    logDs,
    logSearchDs,
    exportDs,
    loadData,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
}));
