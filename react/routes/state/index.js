import React from 'react';
import { PageWrap, PageTab } from '@choerodon/boot';

const KanbanTemplate = React.lazy(() => import('../kanban-template'));
const StateList = React.lazy(() => import('./state-list'));
const TemplateManage = () => (
  <PageWrap
    noHeader={[]}
  >
    <PageTab
      title="状态机"
      route="/agile/states/state"
      tabKey="choerodon.code.organization.setting.issue.states.ps.state"
      component={StateList}
    />
    <PageTab
      title="看板模板"
      route="/agile/states/kanban"
      tabKey="choerodon.code.organization.setting.issue.states.ps.kanban"
      component={KanbanTemplate}
    />
  </PageWrap>
);
export default TemplateManage;
