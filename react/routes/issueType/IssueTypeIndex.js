import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { PermissionRoute } from '@choerodon/master';
import { nomatch } from '@choerodon/boot';

const IssueTypeList = React.lazy(() => import('./issueTypeList'));

const IssueTypeIndex = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={(type) => (type === 'organization' ? [
        'choerodon.code.organization.setting.issue.issue-type.ps.default',
      ] : [
        'choerodon.code.project.setting.issueType.ps.default',
      ])}
      exact
      path={match.url}
      component={IssueTypeList}
    />
    <Route path="*" component={nomatch} />
  </Switch>
);

export default IssueTypeIndex;
