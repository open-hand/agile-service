import React from 'react';
import { useLocation, withRouter } from 'react-router-dom';
import {
  PageWrap, PageTab,
} from '@choerodon/boot';
import { useTabActiveKey } from '@choerodon/components';
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
export default WorkList;
