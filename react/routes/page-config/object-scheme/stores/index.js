import React, { createContext, useMemo } from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import ObjectSchemeStore from './ObjectSchemeStore';

const Store = createContext();
export default Store;

export const StoreProvider = injectIntl(inject('AppState')(
  (props) => {
    const objectSchemeStore = useMemo(() => new ObjectSchemeStore(), []);
    const value = {
      ...props,
      prefixCls: 'issue-object-scheme',
      intlPrefix: 'issue-object-scheme',
      objectSchemeStore,   
    };

    return (
      <Store.Provider value={value}>
        {props.children}
      </Store.Provider>
    );
  },
));
