import React, { createContext, useMemo } from 'react';
// import { store, stores } from '@choerodon/boot';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import StateMachineSchemeStore from './StateMachineSchemeStore';

const Store = createContext();
export default Store;

export const StoreProvider = injectIntl(inject('AppState')(
  (props) => {
    // const { AppState } = props;
    // const { children } = props;
    const stateMachineSchemeStore = useMemo(() => new StateMachineSchemeStore(), []);
    const value = {
      ...props,
      prefixCls: 'issue-type',
      intlPrefix: 'issue-type',
      stateMachineSchemeStore,
    };
    return (
      <Store.Provider value={value}>
        {props.children}
      </Store.Provider>
    );
  },
));
