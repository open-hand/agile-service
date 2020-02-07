import React from 'react';
import { StoreProvider } from './stores';
import CreateField from './CreateField';

export default function Index(props) {
  return (
    <StoreProvider {...props}>
      <CreateField />
    </StoreProvider>
  );
}
