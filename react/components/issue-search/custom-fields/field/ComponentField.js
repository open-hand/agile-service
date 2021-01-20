import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import { Select } from 'choerodon-ui';
import SelectComponent from '@/components/select/select-component';

const { Option } = Select;
function ComponentField({ field, value, onChange }) {
  const [, setValue] = useState(0);
  return (
    <SelectComponent
      key={field.code}
      flat
      value={value || []}
      placeholder={field.name}
      multiple
      maxTagCount={3}
      dropdownMatchSelectWidth={false}
      dropdownMenuStyle={{
        maxWidth: 250,
      }}
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(ComponentField);
