import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import SelectMember from '@/components/select/select-user';

function MemberField({
  field, value, onChange, request, projectId,
}) {
  const { code, name } = field;
  const defaultValue = useMemo(() => value, []);
  return (
    <SelectMember
      projectId={projectId}
      key={code}
      flat
      value={value || []}
      autoQueryConfig={{ selectedUserIds: defaultValue }}
      placeholder={name}
      multiple
      maxTagCount={3}
      request={request}
      dropdownMatchSelectWidth={false}
      clearButton
      onChange={onChange}
      extraOptions={code === 'assigneeId' ? [{ id: '0', realName: '未分配' }] : undefined}
    />
  );
}
export default observer(MemberField);
