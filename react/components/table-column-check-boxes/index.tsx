/* eslint-disable react/require-default-props */
import React, { useMemo, useState, useEffect } from 'react';
import {
  CheckBox, DataSet, Form,
} from 'choerodon-ui/pro/lib';
import { observer, useObservable } from 'mobx-react-lite';
import { CheckBoxProps } from 'choerodon-ui/pro/lib/check-box/CheckBox';
import { FormProps } from 'choerodon-ui/pro/lib/form/Form';
import { fieldApi, pageConfigApi } from '@/api';

interface Props<T, TF> {
  options: Array<{ label: string, value: string }>,
  otherCheckBokProps?: T,
  formProps?: TF,
  name?: string,
  dataSet?: DataSet,
  defaultValue?: any,
  handleChange?: (value: string[] | string, oldValue?: string) => void,
}
interface IConfig {
  name?: string,
  defaultValue?: any,
  options?: Array<{ label: string, value: string, checked?: boolean }>,
  onChange?: (data: string[] | string) => void | boolean,
  componentProps?: any,
}
interface ITableColumnCheckBoxesDataProps {
  checkedOptions: string[],
  options: IConfig['options'],
  actions: { checkAll: () => void, unCheckAll: () => void, check: (val: string) => void }
}
interface ITableColumnCheckBoxesComponentProps {
  options: Required<IConfig>['options'],
  setCheckedOptions: React.Dispatch<React.SetStateAction<string[]>>,
  defaultValue: string[],
  handleChange: (value: string[] | string) => void,
}
export function useTableColumnCheckBoxes(config?: IConfig): [ITableColumnCheckBoxesDataProps, ITableColumnCheckBoxesComponentProps] {
  const [options, setOptions] = useState<IConfig['options']>([]);
  const [checkedOptions, setCheckedOptions] = useState<string[]>([]);
  const onChange = useMemo(() => config?.onChange || (() => null), [config?.onChange]);
  const loadData = async () => {
    pageConfigApi.load().then((res: any) => {
      const { content } = res;
      if (content && Array.isArray(content)) {
        setOptions(content.map((option) => ({ label: option.name, value: option.code })));
      }
    });
  };
  const checkAll = () => {
    setCheckedOptions(options?.map((option) => option.value) || []);
  };
  const unCheckAll = () => {
    setCheckedOptions([]);
  };
  const checkOption = (value: string) => {
    setCheckedOptions([...checkedOptions, value]);
  };
  const handleChange = (value: string[] | string, oldValue: string) => {
    if (value) {
      Array.isArray(value) ? setCheckedOptions(value) : setCheckedOptions([value]);
    }
    onChange(value);
  };
  useEffect(() => {
    if (typeof (config?.defaultValue) !== 'undefined') {
      const defaultArr = [];
      if (typeof (config?.defaultValue) === 'string') {
        defaultArr.push(config.defaultValue);
      }
      if (Array.isArray(config.defaultValue)) {
        defaultArr.push(...config.defaultValue);
      }
      setCheckedOptions(defaultArr);
    }
  }, [config?.defaultValue]);
  useEffect(() => {
    if (config?.options) {
      setOptions(config?.options);
    } else {
      loadData();
    }
  }, [config?.options]);

  const dataProps: ITableColumnCheckBoxesDataProps = {
    checkedOptions,
    options,
    actions: { checkAll, unCheckAll, check: checkOption },
  };
  const componentProps: ITableColumnCheckBoxesComponentProps = {
    options,
    setCheckedOptions,
    defaultValue: checkedOptions,
    handleChange,
    ...config?.componentProps,
  };
  return [dataProps, componentProps];
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
  dataSet: propsDataSet, name = 'exportCodes', options, defaultValue, otherCheckBokProps, formProps, handleChange,
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
          <CheckBox name={name} value={option.value} {...otherCheckBokProps} onChange={handleChange}>
            {option.label}
          </CheckBox>
        )))}
      </div>
    </Form>
  );
}
export default observer(TableColumnCheckBoxes);
