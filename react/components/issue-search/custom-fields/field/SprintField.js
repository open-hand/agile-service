import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import SelectSprint from '@/components/select/select-sprint';

function SprintField({ field, value, onChange }) {
  return (
    <SelectSprint
      key={field.code}
      flat
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
