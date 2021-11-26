import React from 'react';
import { withRouter } from 'react-router-dom';
import { PageWrap, PageTab } from '@choerodon/boot';
import { LoadingProvider } from '@choerodon/components';
import { getMenuType } from '@/utils/common';
import ObjectScheme from './object-scheme';
import './index.less';
import PageIssueType from './page-issue-type';
import PageTemplate from './page-template';
import useFormatMessage from '@/hooks/useFormatMessage';

function PageConfig({ components }: any) {
  const formatMessage = useFormatMessage();
  const tabs = [<PageTab
    title={formatMessage({ id: 'agile.common.field' })}
    tabKey={`choerodon.code.${getMenuType() === 'project' ? '' : 'organization.'}setting.page.field`}
    component={withRouter(ObjectScheme)}
    route="/agile/page/field"
    alwaysShow
  />,
    <PageTab
      title={formatMessage({ id: 'agile.page.config' })}
      tabKey={`choerodon.code.${getMenuType() === 'project' ? '' : 'organization.'}setting.page.scheme`}
      component={withRouter(PageIssueType)}
      route="/agile/page/config"
      alwaysShow
    />];
  if (getMenuType() === 'project') {
    tabs.push(<PageTab
      title={formatMessage({ id: 'agile.page.template' })}
      tabKey="choerodon.code.project.setting.page.ps.template"
      component={withRouter(PageTemplate)}
      route="/agile/page/template"
      alwaysShow
    />);
  }
  return (
    <LoadingProvider className="issue-page-config">
      <PageWrap noHeader={[]}>
        {tabs}
      </PageWrap>
    </LoadingProvider>
  );
}
export default PageConfig;
