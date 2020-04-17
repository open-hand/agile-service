import React from 'react';
import { observer } from 'mobx-react-lite';
import { Select } from 'choerodon-ui';
import { toJS } from 'mobx';
import { configTheme } from '@/common/utils';

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
      value={toJS(value)}
      onChange={onChange}
      placeholder={name}
      mode="multiple"
      style={{ width: 120 }}
      allowClear
    >
      {(fieldOptions || []).map(option => <Option value={String(option.id)}>{option.value}</Option>)}    
    </Select>
  );
}
export default observer(SelectField);
