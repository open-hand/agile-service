import React from 'react';
import { StoreProvider } from '../stores';
import IssueTypeList from './IssueTypeList';

export default function Index(props:any) {
  return (
    <StoreProvider {...props}>
      <IssueTypeList />
    </StoreProvider>
  );
}
