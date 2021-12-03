import React, { useState } from 'react';
import {
  withRouter,
} from 'react-router-dom';
import {
  PageWrap, PageTab, Permission,
} from '@choerodon/boot';
import { mount, has, get } from '@choerodon/inject';
import LINK_URL from '@/constants/LINK_URL';
import useHasDevops from '@/hooks/useHasDevops';
import useCheckCurrentService from './useCheckServerVerison';
import useFormatMessage from '@/hooks/useFormatMessage';

const Release = withRouter(React.lazy(() => import('../Release')));
const Publish = mount('agile:PublishVersion');
const VersionList = ({ match }) => {
  const formatMessage = useFormatMessage('agile.version');
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
            <PageTab title={formatMessage({ id: 'route' })} route={LINK_URL.versionRelease} tabKey="choerodon.code.project.cooperation.project-version.ps.default" component={Release} />

            <PageTab title={formatMessage({ id: 'publish.route' })} route={LINK_URL.versionPublish} tabKey="choerodon.code.project.cooperation.project-version.ps.publish" component={() => Publish} />

          </PageWrap>
        )
      }
    </Permission>
  );
};

const Index = ({ match }) => {
  const { loading, version } = useCheckCurrentService();
  const isShowPublish = has('agile:PublishVersion') && !loading && version !== 'saas-normal';
  return isShowPublish ? <VersionList /> : <Release />;
};
export default Index;
