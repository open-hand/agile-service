import React from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { StoreProvider } from './stores';
import Content from './Content';

const index = (props: {workloadChartDs: DataSet}) => (
  <StoreProvider {...props}>
    <Content />
  </StoreProvider>
);

export default index;
