import { DataSet } from 'choerodon-ui/pro';
import type { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import type { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import React from 'react';
import { observer } from 'mobx-react-lite';
import { IChosenFieldField } from '../chose-field/types';
import ChooseField, { useChoseField } from '../chose-field';
import IssueFilterFormConsumer from './IssueFilterForm';
import IssueFilterFormStoreContextProvider from './stores';
import useIssueFilterFormDataSet from './hooks/useIssueFilterFormDataSet';
import useIssueFilterForm from './hooks/useIssueFilterForm';

interface IIssueFilterFormProps {
  dataSet?: DataSet, // 传入外部dataSet 将放弃组件内创建
  fields?: IChosenFieldField[], // 全部字段 用以保证dataSet内值能正常接收
  chosenFields?: IChosenFieldField[], // 可控已选字段
  onDelete?: (field: IChosenFieldField) => boolean | void,
  // defaultValue?: any,
  defaultVisibleFooterAddBtn?: boolean, // 是否显示默认的添加筛选按钮
  extraRenderFields?: (field: IChosenFieldField, otherComponentProps: Partial<SelectProps> | Partial<DatePickerProps>, { dataSet }: { dataSet: DataSet, useSelectUserForceRefreshHook?: (...other: any) => [any, React.Dispatch<React.SetStateAction<any>>] }) => React.ReactElement | false | null,
  extraFormItems?: IChosenFieldField[],
  formColumns?: number
  needInit?: boolean
}
function DefaultChooseField(chooseConfig: any) {
  const [data, componentProps] = useChoseField({
    ...chooseConfig,
  });
  return {
    data,
    component: <ChooseField {...componentProps} dropDownBtnProps={{ icon: 'add' }} />,
  };
}

const IssueFilterForm: React.FC<IIssueFilterFormProps> = (props) => {
  let { children } = props;
  let defaultFooterProps: any = {};
  if (!children && props.defaultVisibleFooterAddBtn) {
    const { data, component } = DefaultChooseField({ defaultValue: props.chosenFields });
    children = component;
    defaultFooterProps = {
      fields: data.fields,
      chosenFields: data.store.getAllChosenField,
      onDelete: (item: any) => {
        data.store.delChosenFields(item.code);
      },
    };
  }
  return (
    <IssueFilterFormStoreContextProvider footer={children} {...props} {...defaultFooterProps}>
      <IssueFilterFormConsumer />
    </IssueFilterFormStoreContextProvider>
  );
};
IssueFilterForm.defaultProps = {
  dataSet: undefined,
  fields: undefined,
  onDelete: undefined,
  chosenFields: undefined,
  defaultVisibleFooterAddBtn: undefined,
  extraRenderFields: undefined,
  extraFormItems: undefined,
  formColumns: undefined,
  needInit: true,
};
export default observer(IssueFilterForm);
export { useIssueFilterFormDataSet, useIssueFilterForm, IIssueFilterFormProps };
