import React from 'react';
import {
  withRouter, Route, Switch, RouteChildrenProps,
} from 'react-router-dom';

import {
  asyncRouter, PageWrap, PageTab, Permission,
  nomatch,
} from '@choerodon/boot';
import LINK_URL from '@/constants/LINK_URL';

const Release = withRouter(asyncRouter(() => import('../Release')));
const Publish = withRouter(asyncRouter(() => (import('../publish-version'))));
const PublishVersionPreview = React.lazy(() => import('../publish-version/PublishVersionPreview'));
const VersionList = ({ match }) => (
  <Permission service={['choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.createversion']}>
    {
      (permission, loading) => (
        <PageWrap
          noHeader={!loading && permission ? [] : ['choerodon.code.project.cooperation.version-list.ps.release']}
        >
          <PageTab title="规划版本" route={LINK_URL.versionRelease} tabKey="choerodon.code.project.cooperation.version-list.ps.release" component={Release} />

          <PageTab title="发布版本" route={LINK_URL.versionPublish} tabKey="choerodon.code.project.cooperation.version-list.ps.publish" component={Publish} />

        </PageWrap>
      )
    }
  </Permission>
);
const Index = ({ match }) => (
  <Switch>
    <Route exact path={match?.url} component={VersionList} />
    <Route exact path={LINK_URL.versionRelease} component={VersionList} />
    <Route exact path={LINK_URL.versionPublish} component={VersionList} />
    <Route exact path={`${match?.url}/publish/preview/:id`} component={PublishVersionPreview} />
    <Route exact path={`${match?.url}/preview/:id`} component={PublishVersionPreview} />
    <Route path="*" component={nomatch} />
  </Switch>
);
export default Index;
