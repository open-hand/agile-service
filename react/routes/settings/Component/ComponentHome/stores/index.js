import React, { createContext, useMemo } from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { inject } from 'mobx-react';
import { observer } from 'mobx-react-lite';
import ComponentHomeDataSet from './ComponentHomeDataSet';
import useFormatMessage from '@/hooks/useFormatMessage';

const Store = createContext();

export default Store;

export const StoreProvider = inject('AppState')(observer(
  (props) => {
    const { AppState: { currentMenuType: { type, id } }, children } = props;
    const formatMessage = useFormatMessage();
    const dataSet = useMemo(() => new DataSet(ComponentHomeDataSet({ type, id, formatMessage })), [type, id, formatMessage]);
    const value = {
      ...props,
      dataSet,
    };
    return (
      <Store.Provider value={value}>
        {children}
      </Store.Provider>
    );
  },
));
