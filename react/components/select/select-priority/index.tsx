import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { priorityApi } from '@/api';

interface Props {
  priorityId?: number
}

const SelectPriority: React.FC<Props> = forwardRef(({ priorityId, ...otherProps },
  ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'priority',
    textField: 'name',
    valueField: 'id',
    request: () => priorityApi.loadByProject(undefined, [String(priorityId)]),
    paging: false,
  }), [priorityId]);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      clearButton={false}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectPriority;
