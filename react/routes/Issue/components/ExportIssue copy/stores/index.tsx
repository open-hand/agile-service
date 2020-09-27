import React, {
  createContext, useContext, useMemo, useEffect,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { DataSet, Table } from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import { findIndex } from 'lodash';
import useIsInProgram from '@/hooks/useIsInProgram';
import { useChoseFieldStore } from '@/components/chose-field/FieldList';
import { IChosenFieldField } from '@/components/chose-field/types';
import ChoseFieldStore from '@/components/chose-field/store';
import { useTableColumnCheckBoxesDataSet } from '@/components/table-column-check-boxes';
import { useIssueFilterFormDataSet } from '@/components/issue-filter-form';
import { getFilterFormSystemFields } from '../utils';

interface Context {
  tableDataSet: DataSet,
  tableRef: React.RefObject<Table>,
  checkOptions: Array<{ value: string, label: string, order?: string, }>,
  tableColumnCheckBoxesDataSet: DataSet,
  issueFilterFormDataSet: DataSet,
  choseFieldStore: ChoseFieldStore,
  intl: InjectedIntl,
  prefixCls: string,
  chosenFields: Array<IChosenFieldField>,
  fields: IChosenFieldField[],
}
type Props = Omit<Context, 'prefixCls' | 'issueFilterFormDataSet' | 'tableColumnCheckBoxesDataSet'> & { children: React.Component }
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
    const currentChosenFields: Map<string, IChosenFieldField> = new Map();
    fields.forEach((field) => {
      let newField = field;

      if (['time', 'datetime', 'date'].indexOf(field.fieldType ?? '') !== -1) {
        newField = { ...field, otherComponentProps: { range: true } };
      }
      if (field.id) {
        customFields.push(newField);
      } else if (!field.noDisplay) {
        // 冲刺特殊处理
        if (field.code === 'sprint') {
          newField.immutableCheck = true;
          currentChosenFields.set(field.code, newField);
        }
        systemFields.push(newField);
      }
    });
    chosenFields.forEach((field) => {
      let newField = field;
      if (['time', 'datetime', 'date'].indexOf(field.fieldType ?? '') !== -1) {
        newField = { ...field, otherComponentProps: { range: true } };
      }
      // 冲刺特殊处理
      if (field.code === 'sprint') {
        newField.immutableCheck = true;
      }
      currentChosenFields.set(field.code, newField);
    });
    const checkOptions = customFields.map((option) => ({ value: option.code, label: option.name })); // 自定义字段
    const choseFieldStore = useChoseFieldStore({ systemFields, customFields, chosenFields: [...currentChosenFields.values()] });
    const issueFilterFormDataSet = useIssueFilterFormDataSet({ fields, systemFields: getFilterFormSystemFields(isInProgram) });
    const tableColumnCheckBoxesDataSet = useTableColumnCheckBoxesDataSet('exportFieldCodes', columns.map((column) => column.name));

    const value = {
      ...props,
      checkOptions: props.checkOptions.concat(...checkOptions),
      issueFilterFormDataSet,
      choseFieldStore,
      tableColumnCheckBoxesDataSet,
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
