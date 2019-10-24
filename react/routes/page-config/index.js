import React from 'react';
import { Route, Switch } from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';

const PageConfig = asyncRouter(() => (import('./PageIndex')));

const PageIndex = ({ match }) => (
  <Switch>
    <Route path={`${match.url}`} component={PageConfig} />
    <Route path="*" component={nomatch} />
  </Switch>
);
export default PageIndex;
