import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';

const StateList = asyncRouter(() => import('./state-list'));

const StateIndex = (res) => {
  const { match } = res;
  return (
    <Switch>
      <Route path={match.url} component={StateList} />
      <Route path="*" component={nomatch} />
    </Switch>
  );
};

export default StateIndex;
