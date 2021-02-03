import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import SelectEpic from '@/components/select/select-epic';
import { epicApi } from '@/api';

function EpicField({
  field, value, onChange, projectId,
}) {
  return (
    <SelectEpic
      projectId={projectId}
      key={field.code}
      flat
      value={value || []}
      request={() => epicApi.loadEpicsForSelect(projectId)}
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
