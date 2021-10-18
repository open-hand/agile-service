import React from 'react';
import { StoreProvider } from './stores';
import LogIndex from './LogIndex';

const Index = (props: any) => (
  <StoreProvider {...props}>
    <LogIndex />
  </StoreProvider>
);

export default Index;
