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

const Store = createContext({} as Context);

export function useLogStore() {
  return useContext(Store);
}

interface Context {
  logDs: DataSet,
  logSearchDs: DataSet,
  loadData: () => void
  AppState: AppStateProps,
}

export const StoreProvider: React.FC<Context> = inject('AppState')(observer((props: any) => {
  const { children, AppState } = props;
  const logDs = useMemo(() => new DataSet(LogDataSet()), []);
  const logSearchDs = useMemo(() => new DataSet(LogSearchDataSet({ logDs, currentProject: AppState.getCurrentProject })), [AppState.getCurrentProject, logDs]);

  const loadData = useCallback(() => {
    logDs.setQueryParameter('startTime', localPageCacheStore.getItem('workingHours-log-startTime') || `${moment().subtract(31, 'days').format('YYYY-MM-DD HH:mm:ss')}`);
    logDs.setQueryParameter('endTime', localPageCacheStore.getItem('workingHours-log-endTime') || `${moment().format('YYYY-MM-DD HH:mm:ss')}`);
    logDs.setQueryParameter('userIds', localPageCacheStore.getItem('workingHours-log-userIds'));
    logDs.setQueryParameter('projectIds', localPageCacheStore.getItem('workingHours-log-projectIds'));
    logDs.query();
  }, [logDs]);

  useEffect(() => {
    loadData();
  }, [loadData, logDs]);

  const value = {
    ...props,
    logDs,
    logSearchDs,
    loadData,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
}));