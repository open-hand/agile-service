import React, { useState } from 'react';
import { PageWrap, PageTab } from '@choerodon/boot';
import './index.less';
import { usePersistFn } from 'ahooks';
import { LoadingProvider } from '@/components/Loading';

const KanbanTemplate = React.lazy(() => import('../kanban-template/list'));
const StateList = React.lazy(() => import('./state-list'));
const StatusMachineTemplate = React.lazy(() => import('../statusMachine-template'));
const TemplateManage = () => {
  const [activeKey, setActiveKey] = useState('status_change');
  const component = usePersistFn(() => (
    <StatusMachineTemplate
      activeKey={activeKey}
      setActiveKey={setActiveKey}
    />
  ));
  return (
  // <LoadingProvider>
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
        component={component}
      />
      <PageTab
        title="看板模板"
        route="/agile/states/kanban"
        tabKey="choerodon.code.organization.setting.issue.states.ps.kanban"
        component={KanbanTemplate}
      />
    </PageWrap>
  // </LoadingProvider>
  );
};
export default TemplateManage;
