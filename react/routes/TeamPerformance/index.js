import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { nomatch } from '@choerodon/boot';

const TeamPerformanceIndex = React.lazy(() => (import('./TeamPerformanceIndex')));

const ReleaseIndex = ({ match }) => (
  <Switch>
    <Route path={`${match.url}`} component={TeamPerformanceIndex} />
    <Route path="*" component={nomatch} />
  </Switch>
);
export default ReleaseIndex;
