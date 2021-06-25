import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { nomatch } from '@choerodon/boot';
import { PermissionRoute } from '@choerodon/master';

const ReleaseHome = React.lazy(() => (import('./ReleaseHome')));

const ReleaseIndex = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={[
        'choerodon.code.project.cooperation.project-version.ps.default',
        // 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.createversion',
        // 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.worklist.deleteversion',
        // 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.worklist.updateversionstatus',
        // 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.worklist.updateversion',
        // 'choerodon.code.project.cooperation.work-list.ps.version.link.program.version',
      ]}
      path={`${match.url}`}
      component={ReleaseHome}
    />
    <Route path="*" component={nomatch} />
  </Switch>
);

export default ReleaseIndex;
