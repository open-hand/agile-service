import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import SelectPriority from '@/components/select/select-priority';

const list = [];
function PriorityField({ field, value, onChange }) {
  const [, setValue] = useState(0);
  return (
    <SelectPriority
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
