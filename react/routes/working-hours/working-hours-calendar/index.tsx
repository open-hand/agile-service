import React from 'react';
import { StoreProvider } from './stores';
import CalendarIndex from './CalendarIndex';

const Index = (props: any) => (
  <StoreProvider {...props}>
    <CalendarIndex />
  </StoreProvider>
);

export default Index;
