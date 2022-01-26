import React, {
  createContext, useState, useContext, useEffect, useLayoutEffect,
} from 'react';
import { inject } from 'mobx-react';
import { useCreation } from 'ahooks';
import { observer } from 'mobx-react-lite';
import { observable } from 'mobx';
import { includes, omit } from 'lodash';
import { LoadingProvider } from '@choerodon/components';
import useQueryString from '@/hooks/useQueryString';
import useIsInProgram from '@/hooks/useIsInProgram';
import { getMenuType, getApplyType, getIsOrganization } from '@/utils/common';
import type { IStateMachineContext, IStateMachineProps, IStateMachineTab } from '../types';
import useSelectedType from '../useSelectedType';

import CustomCirculation from '../custom-circulation';
import StatusCirculation from '../status-circulation';
import useFormatMessage from '@/hooks/useFormatMessage';
import Status from '../status';
import { statusTransformApi } from '@/api';

const StateMachineContext = createContext({} as IStateMachineContext);
export function useStateMachineContext() {
  return useContext(StateMachineContext);
}
const StateMachineProvider: React.FC<IStateMachineProps> = (props) => {
  const params = useQueryString();
  const {
    defaultTabKeys = ['status', 'status_change', 'custom'],
    readOnly, noContainer, visibleIssueTypeCategory, ...otherProps
  } = props;
  const formatMessage = useFormatMessage('agile.stateMachine');
  const { issueTypeId, activeKey: paramsActiveKey } = params;
  const [selectedType, handleChangeSelectedType] = useSelectedType(issueTypeId || undefined);
  const [issueTypeInitedMap, setIssueTypeInitedMap] = useState<Map<string, boolean>>(observable.map());
  const isOrganization = getIsOrganization();
  const tabs: IStateMachineTab[] = useCreation(() => [{
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
  const defaultTabs = useCreation(() => tabs.filter((item) => includes(defaultTabKeys, item.key)), [defaultTabKeys]);

  const [activeKey, setActiveKey] = useState<IStateMachineContext['activeKey']>(() => {
    if (props.activeKey) {
      return props.activeKey;
    }
    if (paramsActiveKey) {
      return paramsActiveKey;
    }
    return issueTypeId ? defaultTabs[1].key : defaultTabs[0].key;
  });
  const [loading, setLoading] = useState<boolean>(false);
  useLayoutEffect(() => {
    if (props.setActiveKey) {
      props.setActiveKey(activeKey);
    }
  }, [activeKey, props, props.setActiveKey]);

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
  }, [isOrganization, issueTypeInitedMap, selectedType]);
  // useEffect(() => {
  //   if (isOrganization && selectedType && issueTypeInitedMap.has(selectedType) && !issueTypeInitedMap.get(selectedType)) {
  //     propSetActiveKey('status_change');
  //   }
  // }, [activeKey, isOrganization, issueTypeInitedMap, propSetActiveKey, selectedType]);
  const value: IStateMachineContext = useCreation(() => ({
    visibleIssueTypeCategory,
    selectedType,
    isOrganization,
    setSelectedType: handleChangeSelectedType,
    issueTypeInitedMap,
    setIssueTypeInitedMap,
    readOnly: readOnly ?? false,
    noContainer: noContainer ?? false,
    displayTabs: defaultTabs,
    activeKey,
    setActiveKey,
  }), [selectedType, issueTypeInitedMap, defaultTabs, isOrganization]);
  return (
    <StateMachineContext.Provider value={{ ...value, componentProps: omit(otherProps, ['setActiveKey', 'activeKey']) }}>
      <LoadingProvider loading={loading} style={{ width: '100%', height: '100%' }}>
        {props.children}
      </LoadingProvider>
    </StateMachineContext.Provider>
  );
};
export default StateMachineProvider;
