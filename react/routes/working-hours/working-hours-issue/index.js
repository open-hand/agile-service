import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { PermissionRoute } from '@choerodon/master';
import { nomatch } from '@choerodon/boot';

const IssueIndex = React.lazy(() => import('./IssueIndex'));

const WorkingHoursIssue = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={(type) => (type === 'project' ? ['choerodon.code.project.cooperation.working-hours.issue.ps.default'] : [])}
      path={match.url}
      component={IssueIndex}
    />
    <Route path="*" component={nomatch} />
  </Switch>
);

export default WorkingHoursIssue;
