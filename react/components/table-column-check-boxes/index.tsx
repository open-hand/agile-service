import React, {
  useMemo, useState, useEffect, useCallback,
} from 'react';
import {
  DataSet, Form, SelectBox, Tooltip,
} from 'choerodon-ui/pro/lib';
import {
  set, uniq, cloneDeep, noop,
} from 'lodash';
import { observer } from 'mobx-react-lite';
import { CheckBoxProps } from 'choerodon-ui/pro/lib/check-box/CheckBox';
import { FormProps } from 'choerodon-ui/pro/lib/form/Form';
import { pageConfigApi } from '@/api';
import { OptionProps } from 'choerodon-ui/lib/select';
import { usePersistFn } from 'ahooks';

interface OptionItemProps {
  label: string, value: string, checkBoxProps?: CheckBoxProps, defaultChecked?: boolean, optionConfig?: OptionProps
}
interface Props {
  options: Array<OptionItemProps>,
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
function processOptions<T>(options: Array<T> | undefined, events: IConfig['events'] & { changeChecked: React.Dispatch<React.SetStateAction<string[]>> }, ...args: any[]): { options: Array<T>, checkedOptions: Array<string> } {
  let newOptions = cloneDeep(options || []);
  if (events?.initOptions) {
    newOptions = events.initOptions.call(this, ...args) || newOptions;
  }
  const willCheckOptions: string[] = newOptions.filter((option: any) => option.defaultChecked).map((item: any) => item.value);
  willCheckOptions.length > 0 && events.changeChecked((oldCheckedOptions) => {
    const temp = uniq(oldCheckedOptions.concat(willCheckOptions));
    let dataSet: DataSet | undefined;
    let name: string | undefined;
    args.forEach((arg) => {
      if (!dataSet && typeof (arg) === 'object') {
        dataSet = arg.dataSet;
        name = arg.name;
      }
    });
    dataSet && name && dataSet.current?.set(name, temp);
    return uniq(temp);
  });
  return { options: newOptions, checkedOptions: willCheckOptions };
}
export function useTableColumnCheckBoxes(config?: IConfig): [ITableColumnCheckBoxesDataProps, ITableColumnCheckBoxesComponentProps] {
  const [checkedOptions, setCheckedOptions] = useState<string[]>([]);
  const name = useMemo(() => config?.name || 'exportCodes', [config?.name]);
  const form = useMemo(() => ({} as { dataSet: DataSet }), []);
  const onChange = useMemo(() => config?.onChange || (() => null), [config?.onChange]);
  const events = useMemo(() => config?.events, []);

  const [options, setOptions] = useState<ITableColumnCheckBoxesComponentProps['options']>(() => processOptions<OptionItemProps>(config?.options, { ...(events || {}), changeChecked: setCheckedOptions }, {
    options: config?.options, checkedOptions: [], dataSet: form.dataSet, name: config?.name,
  }).options);

  const loadData = useCallback(async () => {
    let newOptions: any = config?.options;
    if (!newOptions) {
      const { content } = await pageConfigApi.load();
      newOptions = content.map((option: any) => ({ label: option.name, value: option.code }));
    }
    setOptions(processOptions<OptionItemProps>(newOptions, { ...(events || {}), changeChecked: setCheckedOptions }, {
      options: config?.options, checkedOptions: [], dataSet: form.dataSet, name: config?.name,
    }).options);
  }, [config?.name, config?.options, events, form.dataSet]);
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
    setCheckedOptions((oldValue: any) => [...oldValue, value]);
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
      setCheckedOptions((oldValue: any) => uniq([...oldValue, ...defaultArr]));
    }
  }, [config?.defaultValue]);
  useEffect(() => {
    // 加载数据
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
  dataSet: propsDataSet, name = 'exportCodes', options, defaultValue, formProps = {}, handleChange: onChange,
}) => {
  const handleChange = usePersistFn(onChange || noop);
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
    let newLabel = label;
    if (label.length > 5) {
      newLabel = `${label.substring(0, 5)}...`;
    }
    return (
      <Tooltip title={label}>
        {newLabel}
      </Tooltip>
    );
  }

  return (
    <Form dataSet={dataSet} {...formProps}>
      <SelectBox name={name} onChange={handleChange}>
        {options.map((option) => <SelectBox.Option key={`table-column-check-box-${option.value}`} value={option.value} {...option.optionConfig}>{renderOptionLabel(option.label)}</SelectBox.Option>)}
      </SelectBox>
    </Form>
  );
};
export default observer(TableColumnCheckBoxes);
