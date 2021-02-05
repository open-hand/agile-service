import React from 'react';
import { observer } from 'mobx-react-lite';
import SelectComponent from '@/components/select/select-component';

function ComponentField({
  field, value, onChange, projectId,
}) {
  return (
    <SelectComponent
      projectId={projectId}
      key={field.code}
      flat
      value={value || []}
      placeholder={field.name}
      multiple
      maxTagCount={3}
      dropdownMatchSelectWidth={false}
      dropdownMenuStyle={{
        maxWidth: 250,
      }}
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(ComponentField);
