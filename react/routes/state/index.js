import React, { useState } from 'react';
import { PageWrap, PageTab, asyncRouter } from '@choerodon/boot';
import './index.less';

const KanbanTemplate = asyncRouter(() => import('../kanban-template/list'));
const StateList = asyncRouter(() => import('./state-list'));
const StatusMachineTemplate = asyncRouter(() => import('../statusMachine-template'));
const TemplateManage = () => {
  const [activeKey, setActiveKey] = useState('status_change');
  return (
    <PageWrap
      noHeader={activeKey === 'custom' ? ['choerodon.code.organization.setting.issue.states.ps.stateMachine'] : []}
      className="c7n-status-pageWrap"
    >
      <PageTab
        title="状态"
        route="/agile/states/state"
        tabKey="choerodon.code.organization.setting.issue.states.ps.state"
        component={StateList}
      />
      <PageTab
        title="状态机模板"
        route="/agile/states/stateMachine"
        tabKey="choerodon.code.organization.setting.issue.states.ps.stateMachine"
        component={() => (
          <StatusMachineTemplate
            activeKey={activeKey}
            setActiveKey={setActiveKey}
          />
        )}
      />
      <PageTab
        title="看板模板"
        route="/agile/states/kanban"
        tabKey="choerodon.code.organization.setting.issue.states.ps.kanban"
        component={KanbanTemplate}
      />
    </PageWrap>
  );
};
export default TemplateManage;
