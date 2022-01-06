import React, {
  useState, ReactNode, useLayoutEffect, useEffect,
} from 'react';
import { Tabs } from 'choerodon-ui';
import { find, includes } from 'lodash';
import { observable } from 'mobx';
import { observer } from 'mobx-react-lite';
import classNames from 'classnames';
import { useCreation } from 'ahooks';
import useQueryString from '@/hooks/useQueryString';
import { getIsOrganization } from '@/utils/common';
import { statusTransformApi } from '@/api';
import Loading, { LoadingHiddenWrap, LoadingProvider } from '@/components/Loading';
import Status from './status';
import StatusCirculation from './status-circulation';
import CustomCirculation from './custom-circulation';
import StateMachineContext from './context';
import useSelectedType from './useSelectedType';
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
const StateMachine: React.FC = ({
  // @ts-ignore
  defaultTabKeys = ['status', 'status_change', 'custom'], readOnly = false, activeKey: propActiveKey, setActiveKey: propSetActiveKey, visibleIssueTypeCategory, noContainer = false, ...otherProps
}) => {
  const formatMessage = useFormatMessage('agile.stateMachine');
  const tabs: ITab[] = useCreation(() => [{
    name: formatMessage({ id: 'state' }),
    key: 'status',
    component: Status,
  }, {
    name: formatMessage({ id: 'flow' }),
    key: 'status_change',
    component: StatusCirculation,
  }, {
    name: formatMessage({ id: 'customFlow' }),
    key: 'custom',
    component: CustomCirculation,
  }], [formatMessage]);

  const defaultTabs = tabs.filter((item) => includes(defaultTabKeys, item.key));
  const params = useQueryString();
  const isOrganization = getIsOrganization();
  const { issueTypeId, activeKey: paramsActiveKey } = params;
  const [selectedType, handleChangeSelectedType] = useSelectedType(issueTypeId || undefined);
  const [issueTypeInitedMap, setIssueTypeInitedMap] = useState<Map<string, boolean>>(observable.map());
  const [loading, setLoading] = useState<boolean>(false);
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

  useEffect(() => {
    if (isOrganization && selectedType && !issueTypeInitedMap.get(selectedType)) {
      setLoading(true);
      statusTransformApi.hasTemplate(selectedType).then((res: boolean) => {
        issueTypeInitedMap.set(selectedType, res);
        setIssueTypeInitedMap(issueTypeInitedMap);
        // if (!res) {
        //   propSetActiveKey('status_change');
        // }
        setLoading(false);
      });
    }
  }, [isOrganization, issueTypeInitedMap, propSetActiveKey, selectedType]);
  // useEffect(() => {
  //   if (isOrganization && selectedType && issueTypeInitedMap.has(selectedType) && !issueTypeInitedMap.get(selectedType)) {
  //     propSetActiveKey('status_change');
  //   }
  // }, [activeKey, isOrganization, issueTypeInitedMap, propSetActiveKey, selectedType]);
  const Component = find(defaultTabs, { key: activeKey })?.component;
  const tabComponent = (
    <Tabs
      className={classNames({
        [styles.tabs]: true,
        [styles.brighterTabs]: readOnly,
      })}
      activeKey={activeKey}
      onChange={setActiveKey}
    >
      {defaultTabs.map((tab) => <TabPane key={tab.key} tab={tab.name} />)}
    </Tabs>
  );

  return (
    <StateMachineContext.Provider value={{
      selectedType,
      setSelectedType: handleChangeSelectedType,
      issueTypeInitedMap,
      setIssueTypeInitedMap,
      readOnly,
      noContainer,
      visibleIssueTypeCategory,
    }}
    >
      <Loading loading={loading} />
      <div
        style={{
          flex: readOnly ? 1 : 0,
        }}
      >
        {
         isOrganization && !issueTypeInitedMap.get(selectedType) ? (
           <LoadingHiddenWrap>
             <NoTemplate activeKey={activeKey} />
           </LoadingHiddenWrap>
         ) : (
           <>
             {Component && <Component {...otherProps} tab={tabComponent} />}
           </>
         )
            }
      </div>

    </StateMachineContext.Provider>
  );
};

export default observer(StateMachine);
