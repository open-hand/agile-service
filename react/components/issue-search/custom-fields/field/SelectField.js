import React from 'react';
import { observer } from 'mobx-react-lite';
import SelectCustomField from '@/components/select/select-custom-field';

function SelectField({ field, value, onChange }) {
  const { name, id } = field;
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
    />
  );
}
export default observer(SelectField);
