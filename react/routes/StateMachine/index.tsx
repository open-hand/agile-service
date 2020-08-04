import React, { useState, ReactNode } from 'react';
import { Tabs } from 'choerodon-ui';
import { find } from 'lodash';
import Status from './status';
import CustomCirculation from './custom-circulation';

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
  component: Status,
}, {
  name: '自定义流转',
  key: 'custom',
  component: CustomCirculation,
}];

const { TabPane } = Tabs;
const StateMachine: React.FC = (props) => {
  const [activeKey, setActiveKey] = useState(tabs[0].key);
  const Component = find(tabs, { key: activeKey })?.component;
  const tabComponent = (
    <Tabs activeKey={activeKey} onChange={setActiveKey}>
      {tabs.map((tab) => <TabPane key={tab.key} tab={tab.name} />)}
    </Tabs>
  );
  return (
    <>
      {Component && <Component {...props} tab={tabComponent} />}
    </>
  );
};

export default StateMachine;
