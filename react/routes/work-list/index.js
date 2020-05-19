import React from 'react';
import { withRouter } from 'react-router-dom';
import { asyncRouter, PageWrap, PageTab } from '@choerodon/boot';

const Backlog = asyncRouter(() => import('../Backlog'));
const Issue = asyncRouter(() => (import('../Issue')));
const Release = asyncRouter(() => (import('../Release')));

const WorkList = ({ match }) => (
  <PageWrap noHeader={[]} cache>
    <PageTab title="待办事项" tabKey="choerodon.code.cooperate.workList.backlog" component={withRouter(Backlog)} alwaysShow />
    <PageTab title="所有问题" tabKey="choerodon.code.cooperate.workList.issue" component={withRouter(Issue)} alwaysShow />
    <PageTab title="版本列表" tabKey="choerodon.code.cooperate.workList.version" component={withRouter(Release)} alwaysShow />
  </PageWrap>  
);
export default WorkList;
