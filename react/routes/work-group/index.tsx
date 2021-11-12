import React from 'react';
import { StoreProvider } from './stores';
import Content from './Content';

const WorkGroupIndex = (props: any) => (
  <StoreProvider {...props}>
    <Content />
  </StoreProvider>
);

export default WorkGroupIndex;
