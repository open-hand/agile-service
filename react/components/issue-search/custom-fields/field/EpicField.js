import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/utils/common';
import SelectEpic from '@/components/select/select-epic';
import { getSelectStyle } from '../utils';

const list = [];
function EpicField({ field, value, onChange }) {
  const [, setValue] = useState(0);
  return (
    <SelectEpic
      key={field.code}
      flat
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
export default observer(EpicField);
