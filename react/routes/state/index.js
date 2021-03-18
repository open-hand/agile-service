import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { asyncRouter, nomatch } from '@choerodon/boot';
import { PermissionRoute } from '@choerodon/master';

const StateList = asyncRouter(() => import('./state-list'));

const StateIndex = (res) => {
  const { match } = res;
  return (
    <Switch>
      <PermissionRoute
        service={[
          'choerodon.code.organization.setting.issue.states.ps.state',
        ]}
        path={match.url}
        component={StateList}
      />
      <Route path="*" component={nomatch} />
    </Switch>
  );
};

export default StateIndex;
