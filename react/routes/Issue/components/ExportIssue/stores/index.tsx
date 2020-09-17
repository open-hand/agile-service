import React, {
  createContext, useContext, useMemo, useEffect,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { DataSet, Table } from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import useIsInProgram from '@/hooks/useIsInProgram';
import { useChoseFieldStore } from '@/components/chose-field/FieldList';
import { IChosenFieldField } from '@/components/chose-field/types';
import ChoseFieldStore from '@/components/chose-field/store';
import ExportIssueDataSet from './ExportIssueDataSet';
import ExportIssueFilterStore from './ExportIssueFilterStore';
import { IExportIssueField, IExportIssueChosenField } from '../types';

interface Context {
  tableDataSet: DataSet,
  tableRef: React.RefObject<Table>,
  checkOptions: Array<{ value: string, label: string, order?: string, }>,
  // exportIssueDataSet: DataSet,
  choseFieldStore: ChoseFieldStore,
  intl: InjectedIntl,
  prefixCls: string,
  chosenFields: Array<IChosenFieldField>,
  fields: IChosenFieldField[],
}
type Props = Omit<Context, 'prefixCls' | 'exportIssueDataSet'> & { children: React.Component }
const ExportIssueContext = createContext({} as Context);

export function useExportIssueStore() {
  return useContext(ExportIssueContext);
}

const ExportIssueContextProvider = injectIntl(observer(
  (props: Props) => {
    const { tableRef, fields, chosenFields } = props;
    const { isInProgram } = useIsInProgram();
    const columns = tableRef.current
      ? tableRef.current.tableStore.columns.filter((column) => column.name && !column.hidden)
      : [];
    const systemFields: Array<IChosenFieldField> = [];
    const customFields: Array<IChosenFieldField> = [];
    const specialFields: Array<IChosenFieldField & { immutableCheck: boolean }> = [];
    fields.forEach((field) => {
      if (field.id) {
        customFields.push(field);
      } else if (!field.noDisplay) {
        // 冲刺特殊处理
        let newField = field;
        if (field.code === 'sprint') {
          newField = { ...field, immutableCheck: true };
          specialFields.push(newField as IChosenFieldField & { immutableCheck: boolean });
        }
        systemFields.push(newField);
      }
    });

    const choseFieldStore = useChoseFieldStore({ systemFields, customFields, chosenFields });
    useEffect(() => {
      // 设置已选中的初始值
      // choseFieldStore.addChosenFields('sprint',)
      specialFields.forEach((field) => {
        choseFieldStore.addSpecialFields(field.code, field);
        console.log('field0', field);
        // exportIssueDataSet.current?.set(field.code, field.value);
      });
    }, []);

    const value = {
      ...props,
      // exportIssueDataSet,
      choseFieldStore,
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
