import React, { useState, ReactNode } from 'react';
import { Tabs } from 'choerodon-ui';
import { find } from 'lodash';
import permission from '@/components/permission';
import useQueryString from '@/hooks/useQueryString';
import Status from './status';
import StatusCirculation from './status-circulation';
import CustomCirculation from './custom-circulation';
import StateMachineContext from './context';
import useSelectedType from './useSelectedType';
import styles from './index.less';

export interface TabComponentProps<Params extends { [K in keyof Params]?: string } = {}> {
  tab: ReactNode
}
interface ITab {
  name: string
  key: string
  component: React.ComponentType<TabComponentProps<any>> | React.ComponentType<any>;
}
const tabs: ITab[] = [{
  name: '状态',
  key: 'status',
  component: Status,
}, {
  name: '状态与流转',
  key: 'status_change',
  component: StatusCirculation,
}, {
  name: '自定义流转',
  key: 'custom',
  component: CustomCirculation,
}];

const { TabPane } = Tabs;
const StateMachine: React.FC = (props) => {
  const params = useQueryString();
  const { issueTypeId } = params;
  const [selectedType, handleChangeSelectedType] = useSelectedType(issueTypeId || undefined);
  const [activeKey, setActiveKey] = useState(issueTypeId ? tabs[1].key : tabs[0].key);
  const Component = find(tabs, { key: activeKey })?.component;
  const tabComponent = (
    <Tabs className={styles.tabs} activeKey={activeKey} onChange={setActiveKey}>
      {tabs.map((tab) => <TabPane key={tab.key} tab={tab.name} />)}
    </Tabs>
  );
  return (
    <StateMachineContext.Provider value={{
      selectedType,
      setSelectedType: handleChangeSelectedType,
    }}
    >
      {Component && <Component {...props} tab={tabComponent} />}
    </StateMachineContext.Provider>
  );
};

export default permission([
  'choerodon.code.project.setting.state.ps.default',
  'choerodon.code.project.setting.state.ps.master',
])(StateMachine);
