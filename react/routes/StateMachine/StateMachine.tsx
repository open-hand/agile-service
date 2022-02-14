import React, {
  ReactNode,
} from 'react';
import { Tabs } from 'choerodon-ui';
import { find } from 'lodash';
import { observer } from 'mobx-react-lite';
import classNames from 'classnames';
import { LoadingHiddenWrap } from '@/components/Loading';
import { useStateMachineContext } from './context';
import NoTemplate from './no-template';
import styles from './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

export interface TabComponentProps<Params extends { [K in keyof Params]?: string } = {}> {
    tab: ReactNode
}
interface ITab {
    name: React.ReactNode
    key: string
    component: React.ComponentType<TabComponentProps<any>> | React.ComponentType<any>;
}

const { TabPane } = Tabs;
const StateMachine: React.FC = () => {
  const formatMessage = useFormatMessage('agile.stateMachine');
  const {
    activeKey, displayTabs, readOnly, setActiveKey, isOrganization, issueTypeInitedMap, selectedType, componentProps,
  } = useStateMachineContext();
  const Component = find(displayTabs, { key: activeKey })?.component;
  const tabComponent = (
    <Tabs
      className={classNames({
        [styles.tabs]: true,
        [styles.brighterTabs]: readOnly,
      })}
      activeKey={activeKey}
      onChange={setActiveKey as any}
    >
      {displayTabs.map((tab) => <TabPane key={tab.key} tab={tab.name} />)}
    </Tabs>
  );

  return (
    <div
      style={{
        flex: readOnly ? 1 : 0,
        height: '100%',
      }}
    >
      {
                isOrganization && !issueTypeInitedMap.get(selectedType) ? (
                  <LoadingHiddenWrap>
                    <NoTemplate activeKey={activeKey} />
                  </LoadingHiddenWrap>
                ) : (
                  <>
                    {Component && <Component {...componentProps} tab={tabComponent} />}
                  </>
                )
            }
    </div>

  );
};
export default observer(StateMachine);
