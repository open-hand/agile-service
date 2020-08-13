import React from 'react';
import { StoreProvider, CreateFiledProps } from './stores';
import CreateField from './CreateField';

export default function Index(props: CreateFiledProps) {
  return (
    // @ts-ignore
    <StoreProvider {...props}>
      <CreateField />
    </StoreProvider>
  );
}
