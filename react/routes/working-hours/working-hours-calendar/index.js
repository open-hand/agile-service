import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { PermissionRoute } from '@choerodon/master';
import { nomatch } from '@choerodon/boot';

const CalendarIndex = React.lazy(() => import('./CalendarIndex'));

const WorkingHoursCalendar = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={(type) => (type === 'organization' ? [
        'choerodon.code.organization.cooperation.working-hours.calendar.ps.default',
      ] : [
        'choerodon.code.project.cooperation.working-hours.calendar.ps.default',
      ])}
      path={match.url}
      component={CalendarIndex}
    />
    <Route path="*" component={nomatch} />
  </Switch>
);

export default WorkingHoursCalendar;
