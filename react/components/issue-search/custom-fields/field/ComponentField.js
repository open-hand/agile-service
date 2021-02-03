import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import { unionBy } from 'lodash';
import { Select } from 'choerodon-ui';
import SelectComponent from '@/components/select/select-component';

function ComponentField({
  field, value, onChange, projectId,
}) {
  return (
    <SelectComponent
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
export default observer(ComponentField);
