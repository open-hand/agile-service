import React from 'react';
import { StoreProvider } from './stores';
import Preview from './Preview';

export default function Index(props) {
  return (
    <StoreProvider {...props}>
      <Preview />
    </StoreProvider>
  );
}
