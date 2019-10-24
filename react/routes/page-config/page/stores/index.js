import React, { createContext, useMemo } from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';

import PageStore from './PageStore';


const Store = createContext();
export default Store;

export const StoreProvider = injectIntl(inject('AppState')(
  (props) => {
    const pageStore = useMemo(() => new PageStore(), []);

    const value = {
      ...props,
      prefixCls: 'issue-page',
      intlPrefix: 'issue-page',
      pageStore,

    };
    return (
      <Store.Provider value={value}>
        {props.children}
      </Store.Provider>
    );
  },
));
