import React, { useContext } from 'react';
import { Route, Switch } from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';
import Store from './stores';

const StateMachineSchemeList = asyncRouter(
  () => import('./state-machine-scheme-list'),
);
const EditStateMachineScheme = asyncRouter(
  () => import('./editStateMachineScheme'),
);
const StateMachineSchemeIndex = () => {
  const { match } = useContext(Store);
  return (
    <Switch>
      <Route path={match.url} component={StateMachineSchemeList} />
      <Route exact path={`${match.url}/edit/:id`} component={EditStateMachineScheme} />
      <Route path={'*'} component={nomatch} />
    </Switch>
  );
}


export default StateMachineSchemeIndex;
