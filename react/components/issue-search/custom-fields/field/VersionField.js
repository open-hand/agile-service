import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import { configTheme } from '@/utils/common';
import SelectVersion from '@/components/select/select-version';
import { getSelectStyle } from '../utils';

function VersionField({ field, value, onChange }) {
  const [, setValue] = useState(0);
  return (
    <SelectVersion
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
export default observer(VersionField);
