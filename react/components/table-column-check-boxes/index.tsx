/* eslint-disable react/require-default-props */
import React, { useMemo } from 'react';
import {
  CheckBox, DataSet, Form,
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
  defaultValue?: any,
}
export function useTableColumnCheckBoxesDataSet(name: string, defaultValue?: any) {
  return useMemo(() => new DataSet({
    autoCreate: true,
    autoQuery: false,
    fields: [{
      name, label: '', multiple: true, defaultValue,
    }],
  }), [defaultValue, name]);
}

function TableColumnCheckBoxes<T extends Partial<CheckBoxProps>, TF extends Partial<FormProps>>({
  dataSet: propsDataSet, name = 'exportCodes', options, defaultValue, otherCheckBokProps, formProps,
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
  }, [defaultValue, name, propsDataSet]);

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
