import React, { useContext } from 'react';
import { withRouter } from 'react-router-dom';
import { PageWrap, PageTab, stores } from '@choerodon/boot';
import ObjectScheme from './object-scheme';
import './pageConfig.less';
import Store from './stores';
import PageIssueType from './page-issue-type';

const { AppState } = stores;

function PageConfig() {
  const { type } = AppState.currentMenuType;
  return (
    <div className="issue-page-config">
      <PageWrap noHeader={[]}>
        <PageTab
          title="字段"
          tabKey={`choerodon.code.${type === 'project' ? '' : 'organization.'}setting.page.field`}
          component={withRouter(ObjectScheme)}
          route="/agile/page/field"
          alwaysShow
        />
        <PageTab
          title="页面配置"
          tabKey={`choerodon.code.${type === 'project' ? '' : 'organization.'}setting.page.scheme`}
          component={withRouter(PageIssueType)}
          route="/agile/page/config"
          alwaysShow
        />
      </PageWrap>
    </div>
  );
}
export default PageConfig;
