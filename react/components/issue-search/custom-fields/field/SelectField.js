import React from 'react';
import { observer } from 'mobx-react-lite';
import { FlatSelect } from '@choerodon/components';
import SelectCustomField from '@/components/select/select-custom-field';

const { Option } = FlatSelect;
function SelectField({ field, value, onChange }) {
  const { fieldOptions, name, id } = field;
  return (
    <SelectCustomField
      fieldId={id}
      flat
      value={value}
      selected={value}
      onChange={onChange}
      placeholder={name}
      dropdownMatchSelectWidth={false}
      multiple
      clearButton
      maxTagCount={3}
      maxTagTextLength={10}
      getPopupContainer={(triggerNode) => triggerNode.parentNode}
    >
      {/* {(fieldOptions || []).map((option) => <Option value={String(option.id)}>{option.value}</Option>)} */}
    </SelectCustomField>
  );
}
export default observer(SelectField);
