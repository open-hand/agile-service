import React, {
  createContext, useContext, useMemo,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { uniq } from 'lodash';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import IssueExportStore from './store';
import { IExportIssueProps } from '..';

interface Context extends IExportIssueProps {
  intl: InjectedIntl,
  prefixCls: string,
  modal?: IModalProps,
}
type Props = Pick<Context, 'intl' | 'tableRef' | 'fields' | 'chosenFields' | 'checkOptions'> & { children: React.Component | React.ReactElement, store?: IssueExportStore };
const ExportIssueContext = createContext({} as Context);

export function useExportIssueStore() {
  return useContext(ExportIssueContext);
}

const ExportIssueContextProvider = injectIntl(observer(
  (props: Props) => {
    const {
      tableRef, fields, chosenFields,
    } = props;
    const columns = useMemo(() => (tableRef.current
      ? tableRef.current.tableStore.columns.filter((column) => column.name && !column.hidden)
      : []), [tableRef]);
    const store = useMemo(() => {
      if (props.store) {
        // 设置默认选项
        props.store.setDefaultCheckedExportFields(uniq(columns.map((column) => column.name!).concat(props.store.defaultCheckedExportFields)));
        // 设置默认选择
        props.store.setDefaultCurrentChosenFields(chosenFields);
        return props.store;
      }
      const newStore = new IssueExportStore({
        defaultCheckedExportFields: columns.map((column) => column.name!),
      });
      newStore.setDefaultCurrentChosenFields(chosenFields);
      return newStore;
    }, [chosenFields, columns, props.store]);
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
