import React, {
  createContext, useContext, useState, useMemo,
} from 'react';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro';

import IssueLinkTableDataSet from './IssueLinkTableDataSet';
import useFormatMessage from '@/hooks/useFormatMessage';

const LinkHomeStore = createContext();
function useLinkHomeStore() {
  return useContext(LinkHomeStore);
}

function LinkHomeStoreComp(props) {
  const { AppState: { currentMenuType: { type, id, organizationId: orgId } }, children } = props;
  const formatMessage = useFormatMessage();
  const issueLinkTableDs = useMemo(() => (new DataSet(IssueLinkTableDataSet({ id, formatMessage }))), [id, formatMessage]);
  const value = {
    ...props,
    formatMessage,
    type,
    id,
    orgId,
    issueLinkTableDs,
  };
  return (
    <LinkHomeStore.Provider value={value}>
      {children}
    </LinkHomeStore.Provider>
  );
}
export default useLinkHomeStore;

export const LinkHomeStoreProvider = inject('AppState')(
  LinkHomeStoreComp,
);
