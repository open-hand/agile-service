import React from 'react';
import { observer } from 'mobx-react-lite';
import { Select } from 'choerodon-ui';
import { configTheme } from '@/utils/common';
import { getSelectStyle } from '../utils';

const { Option } = Select;
function SelectField({ field, value, onChange }) {
  const { fieldOptions, name } = field;
  return (
    <Select
      {...configTheme({
        list: fieldOptions,
        textField: 'value',
        valueFiled: 'id',
        parseNumber: true,
      })}
      value={value}
      onChange={onChange}
      placeholder={name}
      mode="multiple"
      style={getSelectStyle(field, value)}
      dropdownMatchSelectWidth={false}
      allowClear
    >
      {(fieldOptions || []).map(option => <Option value={String(option.id)}>{option.value}</Option>)}    
    </Select>
  );
}
export default observer(SelectField);
