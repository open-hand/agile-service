import React, {
  createContext, useContext, useState,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { observer } from 'mobx-react-lite';
import { IIssueFilterFormProps } from '..';

interface Context extends IIssueFilterFormProps {
  intl: InjectedIntl,
  footer?: React.ReactNode,
  noMemberLoadFinish: boolean,
  setNoMemberLoadFinish: (v: boolean) => void,
  prefixCls: string,
}
type Props = Pick<Context, 'intl' | 'footer'> & { children: React.ReactElement };
const IssueFilterFormStoreContext = createContext({} as Context & { children: React.ReactElement });

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
