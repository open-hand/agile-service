import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';

const ScrumBoardHome = asyncRouter(() => (import('./ScrumBoardHome')));
const Setting = asyncRouter(() => (import('./setting')));

const ScrumBoardIndex = ({ match }) => (
  <Switch>
    <Route exact path={`${match.url}`} component={ScrumBoardHome} />
    <Route path={`${match.url}/setting`} component={Setting} />
    <Route path="*" component={nomatch} />
  </Switch>
);

export default ScrumBoardIndex;
