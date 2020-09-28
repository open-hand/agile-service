import React, {
  createContext, useContext, useMemo, useEffect,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { DataSet, Table } from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import { findIndex } from 'lodash';
import useIsInProgram from '@/hooks/useIsInProgram';
import { IChosenFieldField } from '@/components/chose-field/types';
import { useIssueFilterFormDataSet } from '@/components/issue-filter-form';
import IssueExportStore from './store';

interface Context {
  tableDataSet: DataSet,
  tableRef: React.RefObject<Table>,
  checkOptions: Array<{ value: string, label: string, order?: string, }>,
  issueFilterFormDataSet: DataSet,
  store: IssueExportStore,
  intl: InjectedIntl,
  prefixCls: string,
  chosenFields: Array<IChosenFieldField>,
  fields: IChosenFieldField[],
}
type Props = Pick<Context, 'intl' | 'tableDataSet' | 'tableRef' | 'fields' | 'chosenFields' | 'checkOptions' | 'store'> & { children: React.Component };
const ExportIssueContext = createContext({} as Context);

export function useExportIssueStore() {
  return useContext(ExportIssueContext);
}

const ExportIssueContextProvider = injectIntl(observer(
  (props: Props) => {
    const {
      tableRef, fields, chosenFields,
    } = props;
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
    const issueFilterFormDataSet = useIssueFilterFormDataSet({ fields, systemFields: store.dataSetSystemFields });
    const value = {
      ...props,
      issueFilterFormDataSet,
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
