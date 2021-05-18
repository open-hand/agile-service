import React, { createContext, useContext, useMemo } from 'react';
import { inject } from 'mobx-react';

import { observer } from 'mobx-react-lite';

import { DataSet } from 'choerodon-ui/pro';
import { useProjectOverviewStore } from '@choerodon/master';
import PersonalWorkloadChartDataSet from './PersonalWorkloadChartDataSet';

const Store = createContext();

export function usePersonalWorkloadStore() {
  return useContext(Store);
}

export const StoreProvider = inject('AppState')(observer((props) => {
  const {
    children,
    AppState: { currentMenuType: { projectId, organizationId } },
  } = props;

  const {
    startedRecord,
  } = useProjectOverviewStore();

  const workloadChartDs = useMemo(() => new DataSet(PersonalWorkloadChartDataSet({ projectId, startedRecord, organizationId })), [organizationId, projectId, startedRecord]);

  const value = {
    ...props,
    workloadChartDs,
  };
  return (
    <Store.Provider value={value}>
      {children}
    </Store.Provider>
  );
}));
