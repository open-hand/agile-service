import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { nomatch } from '@choerodon/boot';
import { PermissionRoute } from '@choerodon/master';

const BacklogHome = React.lazy(() => (import('./BacklogHome')));

const BacklogIndex = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={['choerodon.code.project.cooperation.work-list.ps.backlog',
        'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.feature',
        'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint',
        'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.pi',
        'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint',
        'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.epic',
        'choerodon.code.project.cooperation.sprint.work-list.ps.backlog',
        'choerodon.code.project.cooperation.sprint.work-list.ps.backlog.projectupdatesprint',
        'choerodon.code.project.cooperation.sprint.work-list.ps.epic',
      ]}
      path={`${match.url}`}
      component={BacklogHome}
    />
    <Route path="*" component={nomatch} />
  </Switch>
);

export default BacklogIndex;
