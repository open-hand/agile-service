import React, {
  useMemo, useState, useEffect, useCallback,
} from 'react';
import { CheckBox, DataSet, Form } from 'choerodon-ui/pro/lib';
import { set } from 'lodash';
import { observer } from 'mobx-react-lite';
import { CheckBoxProps } from 'choerodon-ui/pro/lib/check-box/CheckBox';
import { FormProps } from 'choerodon-ui/pro/lib/form/Form';
import { pageConfigApi } from '@/api';

interface Props {
  options: Array<{ label: string, value: string, checkBoxProps?: CheckBoxProps }>,
  otherCheckBoxProps?: Partial<CheckBoxProps>,
  formProps?: Partial<FormProps>,
  name?: string,
  dataSet?: DataSet,
  defaultValue?: any,
  handleChange?: (value: string[] | string | undefined, oldValue?: string) => void,
}
interface IConfig {
  name?: string,
  defaultValue?: any,
  options?: Props['options'],
  events?: { initOptions?: (data: { options: Props['options'], checkedOptions: string[], dataSet: DataSet })
  => Props['options'] | void }
  onChange?: (data: string[] | string) => void | boolean,
  checkBoxProps?: Partial<CheckBoxProps>,
}
export interface ITableColumnCheckBoxesDataProps {
  checkedOptions: string[],
  setCheckedOptions: (codes: string[]) => void,
  dataSet: DataSet,
  options: Props['options'],
  actions: { checkAll: () => void, unCheckAll: () => void, check: (val: string) => void }
}
interface ITableColumnCheckBoxesComponentProps extends Props {
}
export function useTableColumnCheckBoxes(config?: IConfig): [ITableColumnCheckBoxesDataProps, ITableColumnCheckBoxesComponentProps] {
  const [options, setOptions] = useState<ITableColumnCheckBoxesComponentProps['options']>([]);
  const [checkedOptions, setCheckedOptions] = useState<string[]>([]);
  const name = useMemo(() => config?.name || 'exportCodes', [config?.name]);
  const form = useMemo(() => ({} as { dataSet: DataSet }), []);
  const onChange = useMemo(() => config?.onChange || (() => null), [config?.onChange]);
  const events = useMemo(() => config?.events, []);
  const loadData = useCallback(async () => {
    let newOptions: any = config?.options;
    if (!newOptions) {
      const { content } = await pageConfigApi.load();
      newOptions = content.map((option: any) => ({ label: option.name, value: option.code }));
    }
    if (events?.initOptions) {
      newOptions = events.initOptions({ options: newOptions, checkedOptions, dataSet: form.dataSet }) || newOptions;
    }
    setOptions(newOptions || []);
  }, [checkedOptions, config?.options, events, form.dataSet]);
  const checkAll = () => {
    const newCheckedOptions = options?.map((option) => option.value) || [];
    form.dataSet.current?.set(name, newCheckedOptions);
    setCheckedOptions(newCheckedOptions);
    onChange(newCheckedOptions);
  };
  const unCheckAll = () => {
    form.dataSet.current?.set(name, []);
    setCheckedOptions([]);
    onChange([]);
  };
  const checkOption = (value: string) => {
    setCheckedOptions([...checkedOptions, value]);
  };
  const handleChange = (value: string[] | string) => {
    if (value) {
      Array.isArray(value) ? setCheckedOptions(value) : setCheckedOptions([value]);
      onChange(Array.isArray(value) ? value : [value]);
    } else {
      setCheckedOptions([]);
      onChange([]);
    }
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
    loadData();
  }, [loadData]);

  const dataProps: ITableColumnCheckBoxesDataProps = {
    checkedOptions,
    setCheckedOptions,
    options,
    dataSet: form.dataSet,
    actions: { checkAll, unCheckAll, check: checkOption },
  };
  const componentProps: ITableColumnCheckBoxesComponentProps = {
    options,
    defaultValue: config?.defaultValue,
    handleChange,
    formProps: form,
    otherCheckBoxProps: config?.checkBoxProps,
    name: config?.name || 'exportCodes',
  };
  return [dataProps, componentProps];
}

const TableColumnCheckBoxes: React.FC<Props> = ({
  dataSet: propsDataSet, name = 'exportCodes', options, defaultValue, otherCheckBoxProps, formProps = {}, handleChange,
}) => {
  const dataSet = useMemo(() => {
    if (propsDataSet) {
      return propsDataSet;
    }
    const newDataSet = new DataSet({
      autoCreate: true,
      autoQuery: false,
      fields: [{
        name, label: '', multiple: true, defaultValue,
      }],
    });
    set(formProps, 'dataSet', newDataSet);
    return newDataSet;
  }, [defaultValue, formProps, name, propsDataSet]);

  return (
    <Form dataSet={dataSet} {...formProps}>
      <div>
        {options.map(((option) => (
          <CheckBox name={name} value={option.value} {...otherCheckBoxProps} onChange={handleChange} {...option.checkBoxProps}>
            {option.label}
          </CheckBox>
        )))}
      </div>
    </Form>
  );
};
export default observer(TableColumnCheckBoxes);
