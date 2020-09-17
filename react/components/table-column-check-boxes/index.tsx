/* eslint-disable react/require-default-props */
import React, {
  createContext, useContext, useMemo, useEffect,
} from 'react';
import { injectIntl, InjectedIntl } from 'react-intl';
import { toJS } from 'mobx';
import {
  CheckBox, DataSet, Form, Table,
} from 'choerodon-ui/pro/lib';
import { observer } from 'mobx-react-lite';
import { CheckBoxProps } from 'choerodon-ui/pro/lib/check-box/CheckBox';
import { FormProps } from 'choerodon-ui/pro/lib/form/Form';

interface Props<T, TF> {
  options: Array<{ label: string, value: string }>,
  otherCheckBokProps?: T,
  formProps?: TF,
  name?: string,
  dataSet?: DataSet,
  onChange?: (value: Array<string>) => void,
  defaultValue?: any,
}
export function useTableColumnCheckBoxesDataSet(name:string, defaultValue?:any) {
  return useMemo(() => new DataSet({
    autoCreate: true,
    autoQuery: false,
    fields: [{
      name, label: '', multiple: true, defaultValue,
    }],
  }), []);
}
function TableColumnCheckBoxes<T extends Partial<CheckBoxProps>, TF extends Partial<FormProps>>({
  dataSet: propsDataSet, name = 'exportCodes', options, defaultValue, otherCheckBokProps, formProps, onChange,
}: Props<T, TF>) {
  const dataSet = useMemo(() => {
    if (propsDataSet) {
      return propsDataSet;
    }
    return new DataSet({
      autoCreate: true,
      autoQuery: false,
      fields: [{
        name, label: '', multiple: true, defaultValue,
      }],
    });
  }, []);
  const handleChangeFieldStatus = (status: 'ALL' | 'NONE') => {
    // if (status !== 'ALL') {
    //   exportIssueDataSet.current?.set('selectedFields', checkOptions.map((column) => column.name));
    // } else {
    //   exportIssueDataSet.current?.set('selectedFields', []);
    // }
    console.log('--');
    return true;
  };
  useEffect(() => {
    if (onChange) {
      onChange(toJS(dataSet.current?.get(name)));
    }
  }, [dataSet.current?.get(name)]);
  return (
    <Form dataSet={dataSet} {...formProps}>
      <div>
        {options.map(((option) => (
          <CheckBox name={name} value={option.value} {...otherCheckBokProps}>
            {option.label}
          </CheckBox>
        )))}
      </div>
    </Form>
  );
}
export default observer(TableColumnCheckBoxes);
