import React from 'react';
import IssueLinkHome from './IssueLinkHome';
import { LinkHomeStoreProvider } from './stores';

function Index(props) {
  return (
    <LinkHomeStoreProvider {...props}>
      <IssueLinkHome />
    </LinkHomeStoreProvider>
  );
}

export default Index;
