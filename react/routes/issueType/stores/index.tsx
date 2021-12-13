import React, { createContext, useMemo } from 'react';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro';
import { AppStateProps } from '@/common/types';
import { getIsOrganization } from '@/utils/common';
import IssueTypeDataSet from './IssueTypeDataSet';
import useFormatMessage from '@/hooks/useFormatMessage';

interface Context {
  AppState: AppStateProps,
  issueTypeDataSet: DataSet,
  isOrganization: boolean,
}

const Store = createContext({} as Context);
export default Store;

export const StoreProvider: React.FC<Context> = inject('AppState')(
  (props) => {
    const isOrganization = getIsOrganization();
    const formatMessage = useFormatMessage();
    const issueTypeDataSet = useMemo(() => new DataSet(IssueTypeDataSet({ isOrganization, formatMessage })), [formatMessage, isOrganization]);
    const value = {
      ...props,
      intlPrefix: 'issue-type',
      issueTypeDataSet,
      isOrganization,
    };
    return (
      <Store.Provider value={value}>
        {props.children}
      </Store.Provider>
    );
  },
);
