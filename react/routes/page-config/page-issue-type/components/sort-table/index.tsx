import React from 'react';
import SortTable from './SortTable';
import SortTableProvider from './stores';

export default function Index(props:any) {
  return (
    <SortTableProvider {...props}>
      <SortTable />
    </SortTableProvider>
  );
}
