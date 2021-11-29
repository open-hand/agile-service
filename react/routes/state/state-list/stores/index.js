import React, { createContext, useMemo } from 'react';
// import { store, stores } from '@choerodon/boot';
import { inject } from 'mobx-react';
import StateStore from './StateStore';

const Store = createContext();
export default Store;

export const StoreProvider = inject('AppState')(
  (props) => {
    // const { AppState } = props;
    // const { children } = props;
    const stateStore = useMemo(() => new StateStore(), []);
    const value = {
      ...props,
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
