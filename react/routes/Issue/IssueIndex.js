/**
 * Created by Qyellow on 2018/4/10.
 */
import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { nomatch } from '@choerodon/boot';
import { PermissionRoute } from '@choerodon/master';

const issueHome = React.lazy(() => (import('./index')));

const IssueIndex = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={[
        'choerodon.code.project.cooperation.work-list.ps.issue',
        'choerodon.code.project.cooperation.sprint.work-list.ps.issue',
      ]}
      path={match.url}
      component={issueHome}
    />
    <Route path="*" component={nomatch} />
  </Switch>
);
export default IssueIndex;
