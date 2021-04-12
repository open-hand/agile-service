import React, {
  createContext, useState, useContext, useMemo,
} from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { DataSet } from 'choerodon-ui/pro/lib';
import PublishVersionDataSet from './PublishVersionDataSet';

interface Context {
  tableDataSet: DataSet
  prefixCls: string
}
const PublishVersionContext = createContext({} as Context);
export function usePublishVersionContext() {
  return useContext(PublishVersionContext);
}
const PublishVersionProvider = injectIntl(inject('AppState')(
  (props: any) => {
    const tableDataSet = useMemo(() => new DataSet(PublishVersionDataSet()), []);
    const value = {
      ...props,
      prefixCls: 'c7n-agile-publish-version',
      tableDataSet,
    };
    return (
      <PublishVersionContext.Provider value={value}>
        {props.children}
      </PublishVersionContext.Provider>
    );
  },
));
export default PublishVersionProvider;
