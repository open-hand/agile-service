import React from 'react';
import { observer } from 'mobx-react-lite';
import SelectPriority from '@/components/select/select-priority';

function PriorityField({
  field, value, onChange, projectId,
}) {
  return (
    <SelectPriority
      projectId={projectId}
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
export default observer(PriorityField);
