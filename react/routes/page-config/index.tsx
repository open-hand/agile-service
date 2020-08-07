import React from 'react';
import { PageConfigProvider } from './stores';
import PageConfig from './PageConfig';

export default function Index(props:any) {
  return (
    <PageConfigProvider {...props}>
      <PageConfig />
    </PageConfigProvider>
  );
}
