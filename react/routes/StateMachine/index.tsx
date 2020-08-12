import React, { useState, ReactNode } from 'react';
import { Tabs } from 'choerodon-ui';
import { stores } from '@choerodon/boot';
import { find } from 'lodash';
import IsProgramContext from '@/hooks/useIsProgrom';
import Status from './status';
import StatusCirculation from './status-circulation';
import CustomCirculation from './custom-circulation';
import StateMachineContext from './context';
import useSelectedType from './useSelectedType';
import styles from './index.less';

const { AppState } = stores;
const { currentMenuType: { category } } = AppState;
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
  const [selectedType, handleChangeSelectedType] = useSelectedType();
  const [activeKey, setActiveKey] = useState(tabs[2].key);
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
      <IsProgramContext.Provider value={{ isProgram: category === 'PROGRAM' }}>
        {Component && <Component {...props} tab={tabComponent} />}
      </IsProgramContext.Provider>
    </StateMachineContext.Provider>
  );
};

export default StateMachine;
