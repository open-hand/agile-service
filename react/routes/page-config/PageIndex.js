import React from 'react';
import { PageConfigProvider } from './stores';
import PageConfig from './PageConfig';

export default function Index(props) {
  return (
    <PageConfigProvider {...props}>
      <PageConfig />
    </PageConfigProvider>
  );
}
