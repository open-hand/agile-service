import React from 'react';
import { StoreProvider } from './stores';
import ObjectScheme from './ObjectScheme';

export default function Index(props:any) {
  return (
    <StoreProvider {...props}>
      <ObjectScheme />
    </StoreProvider>
  );
}
