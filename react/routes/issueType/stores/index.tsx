import React, { createContext, useMemo } from 'react';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro';
import { injectIntl } from 'react-intl';
import { AppStateProps } from '@/common/types';
import IssueTypeDataSet from './IssueTypeDataSet';

interface Context {
  AppState: AppStateProps,
  issueTypeDataSet: DataSet,
}

const Store = createContext({} as Context);
export default Store;

export const StoreProvider: React.FC<Context> = inject('AppState')(
  (props) => {
    const issueTypeDataSet = useMemo(() => new DataSet(IssueTypeDataSet()), []);
    const value = {
      ...props,
      intlPrefix: 'issue-type',
      issueTypeDataSet,
    };
    return (
      <Store.Provider value={value}>
        {props.children}
      </Store.Provider>
    );
  },
);
