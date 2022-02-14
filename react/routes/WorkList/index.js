import React from 'react';
import {
  Route, Switch, useLocation, withRouter,
} from 'react-router-dom';
import {
  PageWrap, PageTab, nomatch,
} from '@choerodon/boot';
import { useTabActiveKey } from '@choerodon/components';
import { PermissionRoute } from '@choerodon/master';
import LINK_URL from '@/constants/LINK_URL';
import useFormatMessage from '@/hooks/useFormatMessage';
import './index.less';

const Backlog = withRouter(React.lazy(() => import('../Backlog')));
const Issue = withRouter(React.lazy(() => (import('../Issue/IssueIndex'))));

const WorkList = ({ match }) => {
  const location = useLocation();
  const formatMessage = useFormatMessage();
  const [, setActiveKey] = useTabActiveKey(location.pathname === '/agile/work-list/issue' ? 'issue' : 'backlog');
  return (
    <PageWrap
      noHeader={[]}
      onChange={setActiveKey}
      className="c7n-agile-workList-PageWrap"
    >
      <PageTab title={formatMessage({ id: 'agile.backlog.route' })} route={LINK_URL.workListBacklog} tabKey="backlog" component={Backlog} />
      <PageTab title={formatMessage({ id: 'agile.issue.route' })} route={LINK_URL.workListIssue} tabKey="issue" component={Issue} />

    </PageWrap>
  );
};

const WorkListIndex = ({ match }) => (
  <Switch>
    <PermissionRoute
      service={[
        'choerodon.code.project.cooperation.work-list.ps.backlog',
        'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.feature',
        'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint',
        'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.pi',
        'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint',
        'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.epic',
        'choerodon.code.project.cooperation.work-list.ps.issue',
      ]}
      path={match.url}
      component={WorkList}
    />
    <Route path="*" component={nomatch} />
  </Switch>
);
export default WorkListIndex;
