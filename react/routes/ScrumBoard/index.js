import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';
import PermissionRoute from '@/components/PermissionRoute';
import { service } from './setting/Setting';

const ScrumBoardHome = asyncRouter(() => (import('./ScrumBoardHome')));
const Setting = asyncRouter(() => (import('./setting')));

const ScrumBoardIndex = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={[...[
        'choerodon.code.project.cooperation.iteration-plan.ps.default',
        'choerodon.code.project.cooperation.iteration-plan.ps.board.create',
        'choerodon.code.project.cooperation.iteration-plan.ps.sprint.finish',
      ], ...service]}
      exact
      path={`${match.url}`}
      component={ScrumBoardHome}
    />
    <Route path={`${match.url}/setting`} component={Setting} />
    <Route path="*" component={nomatch} />
  </Switch>
);

export default ScrumBoardIndex;
