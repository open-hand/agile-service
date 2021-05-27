import React from 'react';
import { observer } from 'mobx-react-lite';
import SelectStatus from '@/components/select/select-status';
import { statusApi } from '@/api';

function StatusField({
  field, value, onChange, projectId, issueTypeIds, applyType = 'agile',
}) {
  return (
    <SelectStatus
      projectId={projectId}
      key={field.code}
      flat
      value={value || []}
      selectedIds={value}
      placeholder="状态"
      multiple
      maxTagCount={3}
      request={() => statusApi.project(projectId).loadByProject(applyType)}
      issueTypeIds={issueTypeIds}
      dropdownMatchSelectWidth={false}
      clearButton
      onChange={onChange}
    />
  );
}
export default observer(StatusField);
