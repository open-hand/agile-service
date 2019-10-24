import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';

const EditStateMachineScheme = asyncRouter(() => import('./state-machine-scheme/editStateMachineScheme'), () => import('../../stores/organization/stateMachineScheme'));
const EditStateMachine = asyncRouter(() => import('./stateMachine/editStateMachine'), () => import('../../stores/organization/stateMachine'));
const EditConfig = asyncRouter(() => import('./stateMachine/editConfig'), () => import('../../stores/organization/stateMachine'));
const EditConfigSelect = asyncRouter(() => import('./stateMachine/editConfigSelect'), () => import('../../stores/organization/stateMachine'));
const State = asyncRouter(() => import('./StateIndex'));

const StateIndex = (res) => {
  const { match } = res;
  return (
    <Switch>
      <Route exact path={`${match.url}/scheme/edit/:id`} component={EditStateMachineScheme} />
      <Route exact path={`${match.url}/state-machine/edit/:id/:status`} component={EditStateMachine} />
      <Route exact path={`${match.url}/state-machine/:machineId/editconfig/:id`} component={EditConfig} />
      <Route exact path={`${match.url}/state-machine/:machineId/editconfig/:id/state/:stateId`} component={EditConfig} />
      <Route exact path={`${match.url}/state-machine/:machineId/editconfig/select/:type/:id`} component={EditConfigSelect} />
      <Route path={match.url} component={State} />
      <Route path="*" component={nomatch} />
    </Switch>
  );
};

export default StateIndex;
