import React from 'react';
import { observer } from 'mobx-react-lite';
import SelectSprint from '@/components/select/select-sprint';

function SprintField({
  field, value, onChange, projectId,
}) {
  return (
    <SelectSprint
      projectId={projectId}
      key={field.code}
      flat
      hasUnassign
      statusList={[]}
      value={value || []}
      placeholder={field.name}
      multiple
      maxTagCount={3}
      maxTagTextLength={10}
      dropdownMatchSelectWidth={false}
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(SprintField);
