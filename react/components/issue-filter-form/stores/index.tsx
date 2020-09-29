import React, {
  createContext, useContext, useMemo, useState,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { DataSet, Table } from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import { findIndex } from 'lodash';
import useIsInProgram from '@/hooks/useIsInProgram';
import { IChosenFieldField } from '@/components/chose-field/types';

interface Context {
    intl: InjectedIntl,
    noMemberLoadFinish: boolean,
    setNoMemberLoadFinish: (v: boolean) => void,
    prefixCls: string,
}
type Props = Pick<Context, 'intl' | 'noMemberLoadFinish'|'setNoMemberLoadFinish'> & { children: React.Component };
const IssueFilterFormStoreContext = createContext({} as Context);

export function useIssueFilterFormStore() {
  return useContext(IssueFilterFormStoreContext);
}
const IssueFilterFormStoreContextProvider = injectIntl(observer(
  (props: Props) => {
    const [noMemberLoadFinish, setNoMemberLoadFinish] = useState<boolean>(false);
    // const issueFilterFormDataSet = useIssueFilterFormDataSet({ fields, systemFields: store.dataSetSystemFields });
    const value = {
      ...props,
      noMemberLoadFinish,
      setNoMemberLoadFinish,
      prefixCls: 'c7n-agile-export-issue-modal',
    };
    return (
      <IssueFilterFormStoreContext.Provider value={value}>
        {props.children}
      </IssueFilterFormStoreContext.Provider>
    );
  },
));
export default IssueFilterFormStoreContextProvider;
