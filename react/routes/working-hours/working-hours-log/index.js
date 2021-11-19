import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { PermissionRoute } from '@choerodon/master';
import { nomatch } from '@choerodon/boot';

const LogIndex = React.lazy(() => import('./LogIndex'));

const WorkingHoursLog = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={(type) => (type === 'project' ? [
        'choerodon.code.project.cooperation.working-hours.log.ps.default',
      ] : [])}
      path={match.url}
      component={LogIndex}
    />
    <Route path="*" component={nomatch} />
  </Switch>
);

export default WorkingHoursLog;
