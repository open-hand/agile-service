import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/utils/common';
import SelectLabel from '@/components/select/select-label';
import { getSelectStyle } from '../utils';

function LabelField({
  field, value, onChange, projectId,
}) {
  return (
    <SelectLabel
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
export default observer(LabelField);
