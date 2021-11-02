import React, {
  createContext, useContext, useMemo,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { uniq } from 'lodash';
import { observer } from 'mobx-react-lite';
import { useCreation } from 'ahooks';
import { IModalProps } from '@/common/types';
import { ITableColumnCheckBoxesOptionData } from '@/components/table-column-check-boxes';
import IssueExportStore from './store';
import { IExportIssueProps } from '..';
import { removeCodeExtraPrefix } from '../utils';

interface Context extends IExportIssueProps {
  intl: InjectedIntl,
  prefixCls: string,
  modal?: IModalProps,
  checkOptions: Array<ITableColumnCheckBoxesOptionData & { order?: any }>
}
type Props = Pick<Context, 'intl' | 'visibleColumns' | 'fields' | 'chosenFields' | 'checkOptions'> & { children: React.Component | React.ReactElement, store?: IssueExportStore };
const ExportIssueContext = createContext({} as Context);

export function useExportIssueStore() {
  return useContext(ExportIssueContext);
}

const ExportIssueContextProvider = injectIntl(observer(
  (props: Props) => {
    const {
      fields, chosenFields, visibleColumns,
    } = props;

    const store = useMemo(() => {
      if (props.store) {
        // 设置默认选项
        props.store.setDefaultCheckedExportFields(uniq(visibleColumns.concat(props.store.defaultCheckedExportFields)).map((code) => removeCodeExtraPrefix(code)));
        // 设置默认选择
        props.store.setDefaultCurrentChosenFields(chosenFields);
        return props.store;
      }
      const newStore = new IssueExportStore({
        defaultCheckedExportFields: visibleColumns,
      });
      newStore.setDefaultCurrentChosenFields(chosenFields);
      return newStore;
    }, [chosenFields, props.store, visibleColumns]);

    const value = {
      ...props,
      // 有概要 则置为显示
      fields: useCreation(() => fields.map((item) => (item.code === 'contents' ? { ...item, noDisplay: false } : item)), [fields]),
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
