import React from 'react';
import PageIssueTypeProvider from './stores';
import PageIssueType from './PageIssueType';

export default function Index(props:any) {
  return (
    <PageIssueTypeProvider {...props}>
      <PageIssueType />
    </PageIssueTypeProvider>
  );
}
