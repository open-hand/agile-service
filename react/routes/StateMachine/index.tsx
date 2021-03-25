import React, { useState, ReactNode, useLayoutEffect } from 'react';
import { Tabs } from 'choerodon-ui';
import { find, includes } from 'lodash';
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
const StateMachine: React.FC = ({
// @ts-ignore
  defaultTabKeys = ['status', 'status_change', 'custom'], activeKey: propActiveKey, setActiveKey: propSetActiveKey, ...otherProps
}) => {
  const defaultTabs = tabs.filter((item) => includes(defaultTabKeys, item.key));
  const params = useQueryString();
  const { issueTypeId, activeKey: paramsActiveKey } = params;
  const [selectedType, handleChangeSelectedType] = useSelectedType(issueTypeId || undefined);
  const [activeKey, setActiveKey] = useState(() => {
    if (propActiveKey) {
      return propActiveKey;
    }
    if (paramsActiveKey) {
      return paramsActiveKey;
    }
    return issueTypeId ? defaultTabs[1].key : defaultTabs[0].key;
  });
  useLayoutEffect(() => {
    if (propSetActiveKey) {
      propSetActiveKey(activeKey);
    }
  }, [activeKey, propSetActiveKey]);
  const Component = find(defaultTabs, { key: activeKey })?.component;
  const tabComponent = (
    <Tabs className={styles.tabs} activeKey={activeKey} onChange={setActiveKey}>
      {defaultTabs.map((tab) => <TabPane key={tab.key} tab={tab.name} />)}
    </Tabs>
  );
  return (
    <StateMachineContext.Provider value={{
      selectedType,
      setSelectedType: handleChangeSelectedType,
    }}
    >
      {Component && <Component {...otherProps} tab={tabComponent} />}
    </StateMachineContext.Provider>
  );
};

export default StateMachine;
