import React from 'react';
import { StoreProvider } from './stores';
import ObjectScheme from './ObjectScheme';

export default function Index(props) {
  return (
    <StoreProvider {...props}>
      <ObjectScheme />
    </StoreProvider>
  );
}
