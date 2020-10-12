import React, {
  createContext, useContext, useMemo, useEffect,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { observer } from 'mobx-react-lite';
import { findIndex } from 'lodash';
import useIsInProgram from '@/hooks/useIsInProgram';
import IssueExportStore from './store';
import { IExportIssueProps } from '..';

interface Context extends IExportIssueProps {
  intl: InjectedIntl,
  prefixCls: string,
}
type Props = Pick<Context, 'intl' | 'tableRef' | 'fields' | 'chosenFields' | 'checkOptions' | 'store'> & { children: React.Component | React.ReactElement };
const ExportIssueContext = createContext({} as Context);

export function useExportIssueStore() {
  return useContext(ExportIssueContext);
}

const ExportIssueContextProvider = injectIntl(observer(
  (props: Props) => {
    const {
      tableRef, fields, chosenFields,
    } = props;
    console.log('fields...', fields);
    const { isInProgram } = useIsInProgram();
    const columns = tableRef.current
      ? tableRef.current.tableStore.columns.filter((column) => column.name && !column.hidden)
      : [];
    const store = useMemo(() => {
      if (props.store) {
        // 设置默认选项
        props.store.setDefaultCheckedExportFields(columns.map((column) => column.name!));
        // 设置默认选择
        props.store.setDefaultCurrentChosenFields(chosenFields);
        return props.store;
      }
      const newStore = new IssueExportStore({
        defaultCheckedExportFields: columns.map((column) => column.name!),
      });
      newStore.setDefaultCurrentChosenFields(chosenFields);
      return newStore;
    }, []);
    // const issueFilterFormDataSet = useIssueFilterFormDataSet({ fields, systemFields: store.dataSetSystemFields });
    const value = {
      ...props,
      store,
      prefixCls: 'c7n-agile-export-issue-modal',
    };
    return (
      <ExportIssueContext.Provider value={value}>
        {props.children}
      </ExportIssueContext.Provider>
    );
  },
));
export default ExportIssueContextProvider;
