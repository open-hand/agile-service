import React, {
  createContext, useContext, useEffect, useMemo,
} from 'react';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro';
import { DataSetSelection } from 'choerodon-ui/pro/lib/data-set/enum';
import DataSetProps from 'choerodon-ui/pro/lib/data-set';
import useStore, { StoreProps } from './useStore';
import { AppStateProps } from '@/common/types';
import TableDataSet from './TableDataSet';

interface ContextProps {
  mainStore: StoreProps,
  MIN_WIDTH: number,
  MAX_WIDTH: number,
  AppState: AppStateProps,
  ROOT_ID: string,
  NOT_ASSIGN_ID: string,
  tableDs: DataSetProps,
}

const Store = createContext({} as ContextProps);

export function useWorkGroupStore() {
  return useContext(Store);
}

export const StoreProvider = inject('AppState')((props: any) => {
  const {
    children,
    AppState: { currentMenuType: { name, id } },
  } = props;

  const ROOT_ID = useMemo(() => `orgId**${id}`, [id]);
  const NOT_ASSIGN_ID = useMemo(() => 'not_assign', []);

  const mainStore = useStore({ ROOT_ID, NOT_ASSIGN_ID });
  const statusDs = useMemo(() => new DataSet({
    data: [{
      text: '启用',
      value: 'true',
    }, {
      text: '停用',
      value: 'false',
    }],
    selection: DataSetSelection.single,
  }), []);
  const tableDs = useMemo(() => new DataSet(TableDataSet({
    statusDs,
    ROOT_ID,
    NOT_ASSIGN_ID,
  })), []);

  useEffect(() => {
    mainStore.loadTreeData();
  }, []);

  const value = {
    ...props,
    mainStore,
    MIN_WIDTH: 240,
    MAX_WIDTH: 600,
    ROOT_ID,
    NOT_ASSIGN_ID,
    tableDs,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
});
