import React from 'react';

import { DataSet } from 'choerodon-ui/pro';
import { StoreProvider } from './stores';
import IssueProgressStatistics from './IssueProgressStatistics';
import IntlProviderAsync from '@/components/intl-provider-async';

const index = (props:any) => (
  <IntlProviderAsync>
    <StoreProvider {...props}>
      <IssueProgressStatistics />
    </StoreProvider>
  </IntlProviderAsync>
);

export default index;
