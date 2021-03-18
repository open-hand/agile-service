import React from 'react';
import { withRouter } from 'react-router-dom';
import {
  asyncRouter, PageWrap, PageTab,
} from '@choerodon/boot';
import LINK_URL from '@/constants/LINK_URL';

const Backlog = withRouter(asyncRouter(() => import('../Backlog')));
const Issue = withRouter(asyncRouter(() => (import('../Issue/IssueIndex'))));

const WorkList = ({ match }) => (
  <PageWrap
    noHeader={[]}
  >
    <PageTab title="待办事项" route={LINK_URL.workListBacklog} tabKey="choerodon.code.project.cooperation.work-list.ps.backlog" component={Backlog} />
    <PageTab title="所有问题" route={LINK_URL.workListIssue} tabKey="choerodon.code.project.cooperation.work-list.ps.issue" component={Issue} />

  </PageWrap>
);
export default WorkList;
