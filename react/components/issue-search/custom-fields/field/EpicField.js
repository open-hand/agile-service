import React from 'react';
import { observer } from 'mobx-react-lite';
import SelectEpic from '@/components/select/select-epic';
import { epicApi } from '@/api';

function EpicField({ field, value, onChange }) {
  return (
    <SelectEpic
      key={field.code}
      flat
      value={value || []}
      request={() => epicApi.loadEpicsForSelect()}
      placeholder={field.name}
      multiple
      unassignedEpic
      maxTagCount={3}
      maxTagTextLength={10}
      dropdownMatchSelectWidth={false}
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(EpicField);
