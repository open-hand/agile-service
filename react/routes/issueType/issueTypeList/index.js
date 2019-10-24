import React from 'react';
import { StoreProvider } from '../stores';
import IssueTypeList from './IssueTypeList';

export default function Index(props) {
  return (
    <StoreProvider {...props}>
      <IssueTypeList />
    </StoreProvider>
  );
}
