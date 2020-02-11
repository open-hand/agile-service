import React, { createContext, useMemo } from 'react';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro';
import { injectIntl } from 'react-intl';
import IssueTypeDataSet from './IssueTypeDataSet';

const Store = createContext();
export default Store;

export const StoreProvider = injectIntl(inject('AppState')(
  (props) => {
    const { AppState: { currentMenuType: { organizationId } }, intl: { formatMessage } } = props;
    const issueTypeDataSet = useMemo(() => new DataSet(IssueTypeDataSet(formatMessage, organizationId)));
    const value = {
      ...props,
      prefixCls: 'agile-issue-type',
      intlPrefix: 'issue-type',
      issueTypeDataSet,
    };
    return (
      <Store.Provider value={value}>
        {props.children}
      </Store.Provider>
    );
  },
));
