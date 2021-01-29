import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import SelectLabel from '@/components/select/select-label';

function LabelField({ field, value, onChange }) {
  return (
    <SelectLabel
      key={field.code}
      flat
      value={value || []}
      placeholder={field.name}
      multiple
      maxTagCount={3}
      dropdownMatchSelectWidth={false}
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(LabelField);
