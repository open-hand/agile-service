import React, {
  createContext, useContext, useEffect, useMemo,
} from 'react';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro';
import DataSetProps from 'choerodon-ui/pro/lib/data-set';
import { IModalProps } from '@/common/types';
import TableDataSet from './TableDataSet';

interface ContextProps {
  tableDs: DataSetProps,
  modal: IModalProps
  workGroupId: string,
  refresh?(): void,
}

const Store = createContext({} as ContextProps);

export function useAddUserStore() {
  return useContext(Store);
}

export const StoreProvider = inject('AppState')((props: any) => {
  const {
    children,
    workGroupId,
  } = props;

  const tableDs = useMemo(() => new DataSet(TableDataSet({ workGroupId })), []);

  const value = {
    ...props,
    tableDs,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
});
