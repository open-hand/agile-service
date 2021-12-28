import React, {
  useMemo, useState, useEffect, useCallback,
} from 'react';
import {
  DataSet, Form, SelectBox, Tooltip, TextField, Icon,
} from 'choerodon-ui/pro';
import { set, uniq } from 'lodash';
import { observer } from 'mobx-react-lite';
import { CheckBoxProps } from 'choerodon-ui/pro/lib/check-box/CheckBox';
import { FormProps } from 'choerodon-ui/pro/lib/form/Form';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/interface';

import { OptionProps } from 'choerodon-ui/lib/select';
import { pageConfigApi } from '@/api';

export interface ITableColumnCheckBoxesOptionData {
  label: string, value: string, checkBoxProps?: CheckBoxProps, defaultChecked?: boolean, optionConfig?: OptionProps
}
interface Props {
  options: Array<ITableColumnCheckBoxesOptionData>,
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
  events?: {
    initOptions?: (data: { options: Props['options'], checkedOptions: string[], dataSet: DataSet })
      => Props['options'] | void
  }
  formProps?: Props['formProps'] // Omit<Pick<> , 'dataSet'>
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
  const [checkedOptions, setCheckedOptions] = useState<string[]>([]);

  const name = useMemo(() => config?.name || 'exportCodes', [config?.name]);
  const form = useMemo(() => ({} as { dataSet: DataSet }), []);
  const onChange = useMemo(() => config?.onChange || (() => null), [config?.onChange]);
  const events = useMemo(() => config?.events, []);
  const handleOptions = (newValue: any) => {
    let newOptions: any = newValue;
    if (events?.initOptions) {
      newOptions = events.initOptions({ options: newOptions, checkedOptions, dataSet: form.dataSet }) || newOptions;
    }
    const willCheckOptions: string[] = newOptions.filter((option: any) => option.defaultChecked).map((item: any) => item.value);
    willCheckOptions.length > 0 && setCheckedOptions((oldCheckedOptions) => {
      const temp = uniq(oldCheckedOptions.concat(willCheckOptions));
      form.dataSet && form.dataSet.current?.set(name, temp);
      return uniq(temp);
    });
    return newOptions || [];
  };
  const [options, setOptions] = useState<ITableColumnCheckBoxesComponentProps['options']>(() => handleOptions(config?.options));

  const loadData = useCallback(async () => {
    let newOptions: any = config?.options;
    if (!newOptions) {
      const { content } = await pageConfigApi.load();
      newOptions = content.map((option: any) => ({ label: option.name, value: option.code }));
    }
    if (events?.initOptions) {
      newOptions = events.initOptions({ options: newOptions, checkedOptions, dataSet: form.dataSet }) || newOptions;
    }
    setOptions(handleOptions(newOptions));
  }, [checkedOptions, config?.options, events, form.dataSet]);
  const minCheckOptionArr = useMemo(() => {
    const willCheckOptions = options.filter((option) => option.optionConfig?.disabled && checkedOptions.includes(option.value)).map((item) => item.value);
    return willCheckOptions;
  }, [checkedOptions, options]);
  const checkAll = () => {
    const newCheckedOptions = options?.filter((option) => !option.optionConfig || (option.optionConfig.disabled && checkedOptions.includes(option.value))).
      map((option) => option.value) || [];
    form.dataSet.current?.set(name, newCheckedOptions);
    setCheckedOptions(newCheckedOptions);
    onChange(newCheckedOptions);
  };
  const unCheckAll = () => {
    form.dataSet.current?.set(name, minCheckOptionArr);
    setCheckedOptions(minCheckOptionArr);
    onChange(minCheckOptionArr);
  };
  const checkOption = (value: string) => {
    setCheckedOptions((oldValue) => [...oldValue, value]);
  };
  const handleChange = (value: string[] | string) => {
    if (value) {
      Array.isArray(value) ? setCheckedOptions(value) : setCheckedOptions([value]);
      onChange(Array.isArray(value) ? value : [value]);
    } else {
      setCheckedOptions(minCheckOptionArr);
      onChange(minCheckOptionArr);
    }
  };
  useEffect(() => {
    if (typeof (config?.defaultValue) !== 'undefined') {
      const defaultArr: string[] = [];
      if (typeof (config?.defaultValue) === 'string') {
        defaultArr.push(config.defaultValue);
      }
      if (Array.isArray(config.defaultValue)) {
        defaultArr.push(...config.defaultValue);
      }
      setCheckedOptions((oldValue) => uniq([...oldValue, ...defaultArr]));
    }
  }, [config?.defaultValue]);
  useEffect(() => {
    // 加载内部数据
    !config?.options && loadData();
  }, [config?.options, loadData]);
  useEffect(() => {
    config?.options && setOptions(handleOptions(config?.options));
  }, [config?.options]);
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
    formProps: { ...form, ...config?.formProps },
    otherCheckBoxProps: config?.checkBoxProps,
    name: config?.name || 'exportCodes',
  };
  return [dataProps, componentProps];
}

const TableColumnCheckBoxes: React.FC<Props> = ({
  dataSet: propsDataSet, name = 'exportCodes', options, defaultValue, formProps = {}, handleChange,
}) => {
  const [filter, setFilter] = useState<string>('');

  const dataSet = useMemo(() => {
    if (propsDataSet) {
      set(formProps, 'dataSet', propsDataSet);
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
  function renderOptionLabel(label: string) {
    const newLabel = label;
    // if (label.length > 5) {
    //   newLabel = `${label.substring(0, 5)}...`;
    // }
    return (
      <Tooltip title={label}>
        {newLabel}
      </Tooltip>
    );
  }

  const handleSearch = useCallback((value) => {
    setFilter(value);
  }, []);

  const filteredOptions = options.filter((option) => option.label?.indexOf(filter || '') > -1);
  return (
    <Form dataSet={dataSet} {...formProps} labelLayout={'none' as LabelLayout} style={{ marginLeft: -5 }}>
      <TextField prefix={<Icon type="search" />} placeholder="请输入搜索内容" style={{ height: 34 }} onChange={handleSearch} clearButton />
      {
        filter && !filteredOptions.length ? (
          <div style={{ color: 'var(--text-color3)' }}>暂无搜索结果</div>
        ) : (
          <SelectBox name={name} onChange={handleChange} style={{ marginTop: -2 }}>
            {filteredOptions.map((option) => <SelectBox.Option value={option.value} {...option.optionConfig}>{renderOptionLabel(option.label)}</SelectBox.Option>)}
          </SelectBox>
        )
      }
    </Form>
  );
};
export default observer(TableColumnCheckBoxes);
