import React, { useState } from 'react';
import {
  withRouter,
} from 'react-router-dom';
import {
  asyncRouter, PageWrap, PageTab, Permission,
} from '@choerodon/boot';
import LINK_URL from '@/constants/LINK_URL';
import { mount, has, get } from '@choerodon/inject';
import useHasDevops from '@/hooks/useHasDevops';
import useCheckCurrentService from './useCheckServerVerison';

const Release = withRouter(asyncRouter(() => import('../Release')));
const Publish = mount('agile:PublishVersion');
const VersionList = ({ match }) => {
  const hasDevops = useHasDevops();
  if (!hasDevops) {
    return <Release />;
  }
  return (
    <Permission service={['choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.createversion']}>
      {
        (permission, loading) => (
          <PageWrap
            noHeader={!loading && permission ? [] : ['choerodon.code.project.cooperation.project-version.ps.default']}
          >
            <PageTab title="规划版本" route={LINK_URL.versionRelease} tabKey="choerodon.code.project.cooperation.project-version.ps.default" component={Release} />

            <PageTab title="发布版本" route={LINK_URL.versionPublish} tabKey="choerodon.code.project.cooperation.project-version.ps.publish" component={() => Publish} />

          </PageWrap>
        )
      }
    </Permission>
  );
};

const Index = ({ match }) => {
  const { loading, version } = useCheckCurrentService();
  const isShowPublish = has('agile:PublishVersion') && !loading && version !== 'sass-normal';
  return isShowPublish ? <VersionList /> : <Release />;
};
export default Index;
