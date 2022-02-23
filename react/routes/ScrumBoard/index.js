import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { nomatch } from '@choerodon/boot';
import { PermissionRoute } from '@choerodon/master';

const ScrumBoardHome = React.lazy(() => (import('./ScrumBoardHome')));
const Setting = React.lazy(() => (import('./setting')));

const ScrumBoardIndex = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={[
        'choerodon.code.project.cooperation.iteration-plan.ps.default',
        'choerodon.code.project.cooperation.iteration-plan.ps.board.create',
        'choerodon.code.project.cooperation.iteration-plan.ps.sprint.finish',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.default',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.board.create',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.sprint.finish',
      ]}
      exact
      path={`${match.url}`}
      component={ScrumBoardHome}
    />
    <PermissionRoute
      service={[
        'choerodon.code.project.cooperation.iteration-plan.ps.config',
        'choerodon.code.project.cooperation.iteration-plan.ps.status.create',
        'choerodon.code.project.cooperation.iteration-plan.ps.column.create',
        'choerodon.code.project.cooperation.iteration-plan.ps.board.delete',
        'choerodon.code.project.cooperation.iteration-plan.ps.board.update',
        'choerodon.code.project.cooperation.iteration-plan.ps.column',
        'choerodon.code.project.cooperation.iteration-plan.ps.status.update',
        'choerodon.code.project.cooperation.iteration-plan.ps.status.delete',
        'choerodon.code.project.cooperation.iteration-plan.ps.work_calendar.update',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.config',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.status.create',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.column.create',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.board.delete',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.board.update',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.column',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.status.update',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.status.delete',
        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.work_calendar.update',
      ]}
      path={`${match.url}/setting`}
      component={Setting}
    />
    <Route path="*" component={nomatch} />
  </Switch>
);

export default ScrumBoardIndex;
