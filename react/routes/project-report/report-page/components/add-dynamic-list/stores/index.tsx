import React, {
  createContext, useContext,
} from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { findIndex } from 'lodash';
import useIsInProgram from '@/hooks/useIsInProgram';
import { useChoseFieldStore } from '@/components/chose-field/FieldList';
import { IChosenFieldField } from '@/components/chose-field/types';
import ChoseFieldStore from '@/components/chose-field/store';
import { useIssueFilterFormDataSet } from '@/components/issue-filter-form';
import { IIssueColumnName } from '@/common/types';
import { getFilterFormSystemFields } from '../utils';

interface Context {
  issueFilterFormDataSet: DataSet,
  choseFieldStore: ChoseFieldStore,
  prefixCls: string,
  chosenFields: Array<IChosenFieldField>,
  fields: IChosenFieldField[],
}
type Props = Pick<Context, 'fields' | 'chosenFields'> & {
  colList: IIssueColumnName[]
}
const ExportIssueContext = createContext({} as Context);

export function useExportIssueStore() {
  return useContext(ExportIssueContext);
}

const ExportIssueContextProvider: React.FC<Props> = (props) => {
  const { fields, chosenFields, colList } = props;
  const { isInProgram } = useIsInProgram();
  const systemFields: Array<IChosenFieldField> = [];
  const customFields: Array<IChosenFieldField> = [];
  fields.forEach((field) => {
    if (field.id) {
      customFields.push(field);
    } else if (!field.noDisplay) {
      // 冲刺特殊处理
      let newField = field;
      if (field.code === 'sprint') {
        const index = findIndex(chosenFields, (f) => f.code === 'sprint');
        if (index !== -1) {
          chosenFields[index].immutableCheck = true;
        } else {
          newField = { ...field, immutableCheck: true };
          chosenFields.push(newField);
        }
      }
      systemFields.push(newField);
    }
  });
  const choseFieldStore = useChoseFieldStore({ systemFields, customFields, chosenFields });
  const issueFilterFormDataSet = useIssueFilterFormDataSet({ fields, systemFields: getFilterFormSystemFields(isInProgram) });
  const value = {
    ...props,
    issueFilterFormDataSet,
    choseFieldStore,
    prefixCls: 'c7n-agile-project-report-modal',
  };
  return (
    <ExportIssueContext.Provider value={value}>
      {props.children}
    </ExportIssueContext.Provider>
  );
};
export default observer(ExportIssueContextProvider);
