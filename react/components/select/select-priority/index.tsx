import React, { useMemo } from 'react';
import { Select } from 'choerodon-ui/pro';
import { loadPriorities } from '@/api/NewIssueApi';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface Props {
  priorityId?: number
}

const SelectPriority: React.FC<Props> = ({ priorityId, ...otherProps }) => {
  const config = useMemo((): SelectConfig => ({
    name: 'priority',
    textField: 'name',
    valueField: 'id',
    request: () => loadPriorities(),
    middleWare: priorities => priorities.filter((priority: any) => priority.enable || priority.id === priorityId),
    paging: false,
  }), [priorityId]);
  const props = useSelect(config);
  return (
    <Select
      clearButton={false}
      {...props}
      {...otherProps}
    />
  );
};
export default SelectPriority;
