import React from 'react';
import { StoreProvider } from './stores';
import CreateField from './CreateField';

export default function Index(props:any) {
  return (
    <StoreProvider {...props}>
      <CreateField />
    </StoreProvider>
  );
}
