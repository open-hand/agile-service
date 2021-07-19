import React from 'react';
import PageTemplate from './PageTemplate';
import PageTemplateProvider from './stores';

export default function Index(props:any) {
  return (
    <PageTemplateProvider {...props}>
      <PageTemplate />
    </PageTemplateProvider>
  );
}
