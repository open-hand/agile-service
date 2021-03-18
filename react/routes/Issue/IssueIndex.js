/**
 * Created by Qyellow on 2018/4/10.
 */
import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';
import { PermissionRoute } from '@choerodon/master';

const issueHome = asyncRouter(() => (import('./index')));

const IssueIndex = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={[
        'choerodon.code.project.cooperation.work-list.ps.issue',
      ]}
      path={match.url}
      component={issueHome}
    />
    <Route path="*" component={nomatch} />
  </Switch>
);
export default IssueIndex;
