import React from 'react';
import PageIssueType from './PageIssueType';
import PageIssueTypeProvider from './stores';

export default function Index(props:any) {
  return (
    <PageIssueTypeProvider {...props}>
      <PageIssueType />
    </PageIssueTypeProvider>
  );
}
