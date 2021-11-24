import React, { createContext, useMemo, useContext } from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { DataSet } from 'choerodon-ui/pro';
import { usePageConfigContext } from '../../stores';
import SchemeTableDataSet from './SchemeTableDataSet';

interface Context {
  prefixCls: 'issue-object-scheme',
  intlPrefix: 'issue-object-scheme',
  intl: any,
  schemeTableDataSet: DataSet,
  schemeCode: string,
}
const Store = createContext({} as Context);
export function useObjectSchemeStore() {
  return useContext(Store);
}

const StoreProvider = injectIntl(
  (props: any) => {
    const contextPageConfig = usePageConfigContext();
    const { objectDetailItem: { schemeCode } } = contextPageConfig;
    const { intl: { formatMessage } } = props;
    const schemeTableDataSet = useMemo(() => new DataSet(SchemeTableDataSet({
      formatMessage,
      schemeCode,
    })), []);

    const value = {
      ...props,
      prefixCls: 'issue-object-scheme',
      intlPrefix: 'issue-object-scheme',
      schemeTableDataSet,
      schemeCode,
    };

    return (
      <Store.Provider value={value}>
        {props.children}
      </Store.Provider>
    );
  },
);
export default StoreProvider;
