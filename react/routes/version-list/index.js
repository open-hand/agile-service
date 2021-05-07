import React from 'react';
import { withRouter } from 'react-router-dom';
import {
  asyncRouter, PageWrap, PageTab, Permission,
} from '@choerodon/boot';
import LINK_URL from '@/constants/LINK_URL';

const Release = withRouter(asyncRouter(() => import('../Release')));
const Publish = withRouter(asyncRouter(() => (import('../publish-version'))));

const WorkList = ({ match }) => (
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
export default WorkList;
