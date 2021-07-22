import React from 'react';
import { Route, Switch, withRouter } from 'react-router-dom';
import { nomatch } from '@choerodon/boot';

const ComponentHome = React.lazy(() => import('./ComponentHome'));

const ComponentIndex = ({ match }) => (
  <Switch>
    <Route exact path={`${match.url}`} component={ComponentHome} />
    <Route path="*" component={nomatch} />
  </Switch>
);

export default withRouter(ComponentIndex);
