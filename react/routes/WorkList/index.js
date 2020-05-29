import React from 'react';
import { withRouter } from 'react-router-dom';
import { asyncRouter, PageWrap, PageTab } from '@choerodon/boot';

const Backlog = asyncRouter(() => import('../Backlog'));
const Issue = asyncRouter(() => (import('../Issue')));
const Release = asyncRouter(() => (import('../Release')));

const WorkList = ({ match }) => (
  <PageWrap
    noHeader={[]}
  >
    <PageTab title="待办事项" route="/agile/work-list/backlog" tabKey="choerodon.code.project.cooperation.work-list.ps.backlog" component={withRouter(Backlog)} />
    <PageTab title="所有问题" route="/agile/work-list/issue" tabKey="choerodon.code.project.cooperation.work-list.ps.issue" component={withRouter(Issue)} />
    <PageTab title="版本列表" route="/agile/work-list/version" tabKey="choerodon.code.project.cooperation.work-list.ps.version" component={withRouter(Release)} />
  </PageWrap>  
);
export default WorkList;
