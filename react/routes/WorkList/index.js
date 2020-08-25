import React from 'react';
import { withRouter } from 'react-router-dom';
import {
  asyncRouter, PageWrap, PageTab, Permission,
} from '@choerodon/boot';
import LINK_URL from '@/constants/LINK_URL';

const Backlog = withRouter(asyncRouter(() => import('../Backlog')));
const Issue = withRouter(asyncRouter(() => (import('../Issue'))));
const Release = withRouter(asyncRouter(() => (import('../Release'))));

const WorkList = ({ match }) => (
  <Permission service={['choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.createversion']}>
    {
      (hasPermission) => (
        <PageWrap
          noHeader={hasPermission ? [] : ['choerodon.code.project.cooperation.work-list.ps.version']}
        >
          <PageTab title="待办事项" route={LINK_URL.workListBacklog} tabKey="choerodon.code.project.cooperation.work-list.ps.backlog" component={Backlog} />
          <PageTab title="所有问题" route={LINK_URL.workListIssue} tabKey="choerodon.code.project.cooperation.work-list.ps.issue" component={Issue} />
          <PageTab title="版本列表" route={LINK_URL.workListVersion} tabKey="choerodon.code.project.cooperation.work-list.ps.version" component={Release} />
        </PageWrap>
      )
    }
  </Permission>
);
export default WorkList;
