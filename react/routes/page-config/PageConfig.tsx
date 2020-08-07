import React, { useContext } from 'react';
import { withRouter } from 'react-router-dom';
import { PageWrap, PageTab, stores } from '@choerodon/boot';
import PageHome from './page/page-home';
import PageDetail from './page/page-detail';
import ObjectScheme from './object-scheme';
import './pageConfig.less';
import Store from './stores';
import PageIssueType from './page-issue-type';

const { AppState } = stores;

function PageConfig() {
  const context = useContext(Store);
  const { pageDetailVisible } = context;
  const { type } = AppState.currentMenuType;
  return (
    <div className="issue-page-config">
      {pageDetailVisible
        ? <PageDetail />
        : (
          <PageWrap cache noHeader={`choerodon.code.${type === 'project' ? '' : 'organization.'}setting.page.scheme`}>
            <PageTab title="字段" tabKey={`choerodon.code.${type === 'project' ? '' : 'organization.'}setting.page.field`} component={withRouter(ObjectScheme)} alwaysShow />
            <PageTab title="页面配置" tabKey={`choerodon.code.${type === 'project' ? '' : 'organization.'}setting.page.scheme111`} component={withRouter(PageIssueType)} alwaysShow />
            <PageTab title="页面管理" tabKey={`choerodon.code.${type === 'project' ? '' : 'organization.'}setting.page.scheme`} component={withRouter(PageHome)} alwaysShow />
          </PageWrap>
        )}
    </div>
  );
}
export default PageConfig;
