import SelectEnvironment from '@/components/select/select-environment';
import { observer } from 'mobx-react-lite';
import React from 'react';

function EnvironmentField({ field, value, onChange }) {
  return (
    <SelectEnvironment
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
export default observer(EnvironmentField);
