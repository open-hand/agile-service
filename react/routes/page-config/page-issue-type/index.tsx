import React from 'react';
import { PageConfigProvider } from '../stores';
import PageIssueType from './PageIssueType';

export default function Index(props:any) {
  return (
    <PageConfigProvider {...props}>
      <PageIssueType />
    </PageConfigProvider>
  );
}
