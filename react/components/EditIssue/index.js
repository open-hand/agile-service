import React from 'react';
import ErrorBoundary from '../ErrorBoundary';
import EditIssue from './EditIssue';
import { EditIssueContextProvider } from './stores';

export default function Index(props) {
  return (
    <ErrorBoundary>
      <EditIssueContextProvider {...props}>
        <EditIssue />
      </EditIssueContextProvider>
    </ErrorBoundary>
  );
}
