import React, { createContext, useMemo } from 'react';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro';
import { DataSetSelection } from 'choerodon-ui/dataset/data-set/enum';
import { getStageList } from '@/utils/stateMachine';
import StateStore from './StateStore';
import TableDataSet from './TableDataSet';
import useFormatMessage from '@/hooks/useFormatMessage';

interface ContextProps {
  tableDs: DataSet,
  prefixCls: string,
  intlPrefix: string,
  stateStore: any,
}

const Store = createContext({} as ContextProps);
export default Store;

export const StoreProvider = inject('AppState')(
  (props: any) => {
    const formatMessage = useFormatMessage();
    const stageDs = useMemo(() => new DataSet({
      data: getStageList(),
      selection: DataSetSelection.single,
    }), [getStageList]);
    const tableDs = useMemo(() => new DataSet(TableDataSet({ stageDs, formatMessage })), []);
    const stateStore = useMemo(() => new StateStore(), []);
    const value = {
      ...props,
      tableDs,
      prefixCls: 'issue-type',
      intlPrefix: 'issue-type',
      stateStore,
    };
    return (
      <Store.Provider value={value}>
        {props.children}
      </Store.Provider>
    );
  },
);
