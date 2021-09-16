import React, {
  forwardRef, useState, useCallback, useRef,
} from 'react';
import type { ChangeEvent } from 'react';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { Select } from 'choerodon-ui/pro';

const { Option } = Select;

interface Props extends Partial<SelectProps> {
  selectNumbers?: string[],
}

const SelectNumber: React.FC<Props> = forwardRef(({
  selectNumbers = ['0.5', '1', '2', '3', '4', '5', '8', '13'],
  onChange,
  onBlur,
  pattern = /(^\d{1,3}\.{1}\d{1}$)|(^[1-9]\d{0,2}$)/,
  validationRenderer = () => (
    <span>请输入小于3位的整数或者整数位小于3位小数点后一位的小数</span>
  ),
  defaultValue,
  name,
  ...otherProps
}, ref: React.Ref<Select>) => {
  const [value, setValue] = useState<string>('');

  const handleChange = (newValue: string, oldValue: string) => {
    setValue(newValue);
    if (onChange) {
      onChange(newValue, oldValue);
    }
  };

  const handleInput = (e: ChangeEvent<HTMLInputElement>) => {
    if (e && e.target) {
      const newValue = e.target.value;
      if (!defaultValue) {
        setValue(newValue);
      }
    }
  };

  const handleBlur = (e: ChangeEvent<HTMLInputElement>) => {
    if (onChange) {
      onChange(e.target.value, value);
    }
  };

  const handBindRef = useCallback((r: Select) => {
    ref && Object.assign(ref, {
      current: r,
    });
    // 为解决dataset 下组件上设置的正则无效
    name && r?.dataSet?.getField(name)?.set('pattern', pattern);
  }, [name, pattern, ref]);
  return (
    <Select
      ref={handBindRef}
      name={name}
      value={value}
      combo
      searchable
      clearButton={false}
      onChange={handleChange}
      onInput={handleInput}
      onBlur={handleBlur}
      pattern={pattern}
      validationRenderer={validationRenderer}
      {...otherProps}
    >
      {
        selectNumbers.map((number: string) => (
          <Option key={number} value={number}>{number}</Option>
        ))
      }
    </Select>
  );
});
export default SelectNumber;
