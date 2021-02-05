import React from 'react';
import { observer } from 'mobx-react-lite';
import SelectIssueType from '@/components/select/select-issue-type-pro';

function IssueTypeField({
  field, value, onChange, projectId, applyType,
}) {
  return (
    <SelectIssueType
      config={{
        projectId,
        applyType,
      }}
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
export default observer(IssueTypeField);
