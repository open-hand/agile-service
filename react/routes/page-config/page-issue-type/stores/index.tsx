import React, {
  createContext, useState, useContext, useMemo,
} from 'react';

import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import SortTableDataSet from './SortTableDataSet';

interface Context {
    sortTableDataSet: DataSet,
}
const PageIssueTypeContext = createContext({} as Context);

export function usePageIssueTypeStore() {
  return useContext(PageIssueTypeContext);
}

const PageIssueTypeProvider = inject('AppState')(observer(
  (props: any) => {
    const sortTableDataSet = useMemo(() => new DataSet(SortTableDataSet()), []);
    const value = {
      ...props,
      sortTableDataSet,
    };
    return (
      <PageIssueTypeContext.Provider value={value}>
        {props.children}
      </PageIssueTypeContext.Provider>
    );
  },
));
export default PageIssueTypeProvider;
