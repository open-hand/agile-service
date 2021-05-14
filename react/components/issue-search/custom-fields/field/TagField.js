import SelectMultiServiceTag from '@/components/select/select-multi-service-tag';
import { observer } from 'mobx-react-lite';
import React from 'react';

function TagField({ field, value, onChange }) {
  return (
    <SelectMultiServiceTag
      key={field.code}
      placeholder={field.name}
      value={value || []}
      flat
      multiple
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(TagField);
