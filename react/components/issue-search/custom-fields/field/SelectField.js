import React from 'react';
import { observer } from 'mobx-react-lite';
import { FlatSelect } from '@choerodon/components';

const { Option } = FlatSelect;
function SelectField({ field, value, onChange }) {
  const { fieldOptions, name } = field;
  return (
    <FlatSelect
      key={`${field.code}-${value}`}
      value={value}
      onChange={onChange}
      placeholder={name}
      dropdownMatchSelectWidth={false}
      multiple
      clearButton
      maxTagCount={3}
      maxTagTextLength={10}
      getPopupContainer={(triggerNode) => triggerNode.parentNode}
    >
      {(fieldOptions || []).map((option) => <Option value={String(option.id)}>{option.value}</Option>)}
    </FlatSelect>
  );
}
export default observer(SelectField);
