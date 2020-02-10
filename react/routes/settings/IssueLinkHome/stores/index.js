import React, {
  createContext, useContext, useState, useMemo, 
} from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { DataSet } from 'choerodon-ui/pro';

import IssueLinkTableDataSet from './IssueLinkTableDataSet';

const LinkHomeStore = createContext();
function useLinkHomeStore() {
  return useContext(LinkHomeStore);
}


function LinkHomeStoreComp(props) {
  const { intl: { formatMessage }, AppState: { currentMenuType: { type, id, organizationId: orgId } }, children } = props;
  
  const issueLinkTableDs = useMemo(() => (new DataSet(IssueLinkTableDataSet({ id, formatMessage }))), [id]);
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

export const LinkHomeStoreProvider = injectIntl(inject('AppState')(
  LinkHomeStoreComp,
));
