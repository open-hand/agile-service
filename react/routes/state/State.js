import React from 'react';
import { withRouter } from 'react-router-dom';
import { PageWrap, PageTab } from '@choerodon/boot';
import StateList from './state-list';
import StateMachineSchemeList from './state-machine-scheme';
import StateMahine from './stateMachine';
import './State.less';

function State() {
  return (
    <PageWrap noHeader={['choerodon.code.organization.setting.stateMachine.scheme']} cache>
      <PageTab title="状态" tabKey="choerodon.code.organization.setting.stateMachine.state" component={withRouter(StateList)} alwaysShow />
      <PageTab title="状态机" tabKey="choerodon.code.organization.setting.stateMachine.machine" component={withRouter(StateMahine)} alwaysShow />
      <PageTab title="状态机方案" tabKey="choerodon.code.organization.setting.stateMachine.scheme" component={withRouter(StateMachineSchemeList)} alwaysShow />
    </PageWrap>

  );
}
export default State;
