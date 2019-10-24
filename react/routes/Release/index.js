import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';

const ReleaseHome = asyncRouter(() => (import('./ReleaseHome')));

const ReleaseIndex = ({ match }) => (
  <Switch>
    <Route path={`${match.url}`} component={ReleaseHome} />
    <Route path={'*'} component={nomatch} />
  </Switch>
);

export default ReleaseIndex;
