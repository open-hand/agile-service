import React, {
  createContext, useCallback, useContext, useEffect, useMemo, useState,
} from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { inject } from 'mobx-react';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import { AppStateProps } from '@/common/types';
import SelectUserStore from './SelectUserStore';
import UserListDataSet from './UserListDataSet';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getIsOrganization } from '@/utils/common';

const Store = createContext({} as Context);

export function useSelectUserStore() {
  return useContext(Store);
}

interface Context {
  AppState: AppStateProps,
  userListDs: DataSet
  selectUserStore: SelectUserStore,
}

export const StoreProvider: React.FC<Context> = inject('AppState')(observer((props: any) => {
  const {
    children,
    AppState: { currentMenuType: { id } },
  } = props;

  const ROOT_ID = useMemo(() => `orgId**${id}`, [id]);
  const NOT_ASSIGN_ID = useMemo(() => 'not_assign', []);

  const selectUserStore = useMemo(() => new SelectUserStore({
    ROOT_ID,
    NOT_ASSIGN_ID,
  }), [NOT_ASSIGN_ID, ROOT_ID]);
  const userListDs = useMemo(() => new DataSet(UserListDataSet({
    selectUserStore,
    ROOT_ID,
    NOT_ASSIGN_ID,
  })), [NOT_ASSIGN_ID, ROOT_ID, selectUserStore]);

  const initCaseSelected = useMemo(() => ([]), []);
  useEffect(() => {
    selectUserStore.loadIssueTree(initCaseSelected);
  }, [initCaseSelected, selectUserStore]);

  useEffect(() => {
    const { currentCycle } = selectUserStore;
    // @ts-ignore
    const { id: folderId } = currentCycle;
    userListDs.setQueryParameter('workGroupId', folderId);
    userListDs.query();
  }, [selectUserStore, userListDs]);

  const value = {
    ...props,
    userListDs,
    selectUserStore,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
}));
