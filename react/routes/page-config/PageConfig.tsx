import React from 'react';
import { withRouter } from 'react-router-dom';
import { PageWrap, PageTab } from '@choerodon/boot';
import { getMenuType } from '@/utils/common';
import ObjectScheme from './object-scheme';
import './index.less';
import PageIssueType from './page-issue-type';

function PageConfig() {
  return (
    <div className="issue-page-config">
      <PageWrap noHeader={[]}>
        <PageTab
          title="字段"
          tabKey={`choerodon.code.${getMenuType() === 'project' ? '' : 'organization.'}setting.page.field`}
          component={withRouter(ObjectScheme)}
          route="/agile/page/field"
          alwaysShow
        />
        <PageTab
          title="页面配置"
          tabKey={`choerodon.code.${getMenuType() === 'project' ? '' : 'organization.'}setting.page.scheme`}
          component={withRouter(PageIssueType)}
          route="/agile/page/config"
          alwaysShow
        />
      </PageWrap>
    </div>
  );
}
export default PageConfig;
