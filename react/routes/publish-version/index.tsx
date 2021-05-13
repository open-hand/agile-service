import React from 'react';
import Provider from './stores';
import PublishVersion from './PublishVersion';

export default function Index(props:any) {
  return (
    <Provider {...props}>
      <PublishVersion />
    </Provider>
  );
}
