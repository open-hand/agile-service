import React, { createContext, useMemo } from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { inject } from 'mobx-react';
import { observer } from 'mobx-react-lite';
import { injectIntl } from 'react-intl';
import ComponentHomeDataSet from './ComponentHomeDataSet';

const Store = createContext();

export default Store;

export const StoreProvider = injectIntl(inject('AppState')(observer(
  (props) => {
    const { AppState: { currentMenuType: { type, id } }, intl, children } = props;
    const dataSet = useMemo(() => new DataSet(ComponentHomeDataSet({ type, id, intl })), [type, id]);
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
)));
