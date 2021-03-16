import SelectAppVersion from '@/components/select/select-app-version';
import { observer } from 'mobx-react-lite';
import React from 'react';

function AppVersionField({ field, value, onChange }) {
  return (
    <SelectAppVersion
      key={field.code}
      placeholder={field.name}
      value={value || []}
      flat
      multiple
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(AppVersionField);
