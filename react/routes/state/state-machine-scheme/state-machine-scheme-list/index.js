import React, { createContext, useMemo, useState } from 'react';
import { StoreProvider } from '../stores';
import StateMachineSchemeList from './StateMachineSchemeList';

export default function Index(props) {
  return (
    <StoreProvider {...props}>
      <StateMachineSchemeList />
    </StoreProvider>
  );
}
