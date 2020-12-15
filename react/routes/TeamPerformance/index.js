import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';

const TeamPerformanceIndex = asyncRouter(() => (import('./TeamPerformanceIndex')));

const ReleaseIndex = ({ match }) => (
  <Switch>
    <Route path={`${match.url}`} component={TeamPerformanceIndex} />
    <Route path="*" component={nomatch} />
  </Switch>
);
export default ReleaseIndex;
