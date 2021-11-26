import React, { createContext, useMemo, useContext } from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { usePageConfigContext } from '../../stores';
import SchemeTableDataSet from './SchemeTableDataSet';
import useFormatMessage from '@/hooks/useFormatMessage';

interface Context {
  prefixCls: 'issue-object-scheme',
  intlPrefix: 'issue-object-scheme',
  schemeTableDataSet: DataSet,
  schemeCode: string,
}
const Store = createContext({} as Context);
export function useObjectSchemeStore() {
  return useContext(Store);
}

const StoreProvider = (props: any) => {
  const contextPageConfig = usePageConfigContext();
  const { objectDetailItem: { schemeCode } } = contextPageConfig;
  const formatMessage = useFormatMessage();
  const schemeTableDataSet = useMemo(() => new DataSet(SchemeTableDataSet({
    formatMessage,
    schemeCode,
  })), [formatMessage, schemeCode]);

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
};

export default StoreProvider;
