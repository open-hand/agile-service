import React from 'react';
import {
  Route,
  Switch,
} from 'react-router-dom';
import { nomatch } from '@choerodon/boot';
import { PermissionRoute } from '@choerodon/master';

const StoryMapHome = React.lazy(() => (import('./StoryMapHome')));
// const StoryMapSetting = React.lazy(() => (import('./StoryMapSetting')));

const StoryMapIndex = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={[
        'choerodon.code.project.cooperation.story-map.ps.default',
        'choerodon.code.project.cooperation.sprint.story-map.ps.default',
      ]}
      exact
      path={`${match.url}`}
      component={StoryMapHome}
    />
    {/* <Route path={`${match.url}/setting`} component={StoryMapSetting} /> */}
    <Route path="*" component={nomatch} />
  </Switch>
);

export default StoryMapIndex;
