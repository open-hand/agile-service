import React, { createContext } from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';

const Store = createContext();

export default Store;

export const StoreProvider = injectIntl(inject('AppState')(
  (props) => {
    const { children } = props;
    const value = {
      ...props,
      prefixCls: 'c7n-agile-preview',
    };
    return (
      <Store.Provider value={value}>
        {children}
      </Store.Provider>
    );
  },
));
