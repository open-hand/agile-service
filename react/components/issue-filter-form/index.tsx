import { DataSet } from 'choerodon-ui/pro/lib';
import React from 'react';
import { IChosenFieldField } from '../chose-field/types';
import IssueFilterForm, { useIssueFilterFormDataSet, useIssueFilterForm } from './IssueFilterForm';
import IssueFilterFormStoreContextProvider from './stores';

interface Props {
  dataSet?: DataSet, // 传入外部dataSet 将放弃组件内创建
  fields?: IChosenFieldField[], // 全部字段 用以保证dataSet内值能正常接收
  chosenFields?: IChosenFieldField[], // 可控已选字段
  onDelete?: (field: IChosenFieldField) => boolean | void,
  // defaultValue?: any,
  extraFormItems?: IChosenFieldField[],
}
const Index: React.FC<Props> = (props) => (
  // @ts-ignore
  <IssueFilterFormStoreContextProvider {...props}>
    {/** @ts-ignore */}
    <IssueFilterForm {...props} />
  </IssueFilterFormStoreContextProvider>
);
export default Index;
export { useIssueFilterFormDataSet, useIssueFilterForm };
