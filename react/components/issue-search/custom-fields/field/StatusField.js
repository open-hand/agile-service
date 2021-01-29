import React from 'react';
import { observer } from 'mobx-react-lite';
import SelectStatus from '@/components/select/select-status';
import { statusApi } from '@/api';

function StatusField({ field, value, onChange }) {
  return (
    <SelectStatus
      key={field.code}
      flat
      value={value || []}
      placeholder="状态"
      multiple
      maxTagCount={3}
      request={() => statusApi.loadByProject('agile')}
      dropdownMatchSelectWidth={false}
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(StatusField);
