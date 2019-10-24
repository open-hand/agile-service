import React, { createContext, useMemo, useState } from 'react';
import { StoreProvider } from './stores';
import StateMachineSchemeIndex from './StateMachineSchemeIndex';

export default function Index(props) {
  return (
    <StoreProvider {...props}>
      <StateMachineSchemeIndex />
    </StoreProvider>
  );
}
