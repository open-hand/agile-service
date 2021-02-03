import React, { useState } from 'react';
import { observer } from 'mobx-react-lite';
import SelectVersion from '@/components/select/select-version';

function VersionField({
  field, value, onChange, projectId, ...props
}) {
  return (
    <SelectVersion
      projectId={projectId}
      hasUnassign
      key={field.code}
      flat
      value={value || []}
      placeholder={field.name}
      multiple
      maxTagCount={3}
      dropdownMatchSelectWidth={false}
      clearButton
      onChange={onChange}
      valueField="versionId"
      onBlur={() => {
        console.log('blur');
      }}
    />
  );
}
export default observer(VersionField);
