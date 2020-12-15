import React from 'react';
import { StoreProvider } from './stores';
import Performance from './Performance';

export default function Index(props) {
  return (
    <StoreProvider {...props}>
      <Performance />
    </StoreProvider>
  );
}
