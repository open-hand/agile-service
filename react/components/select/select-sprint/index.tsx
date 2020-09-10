import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { Tooltip } from 'choerodon-ui';
import { sprintApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface Props {
  statusList?: string[]
}

const SelectSprint: React.FC<Props> = forwardRef(({ statusList = ['sprint_planning', 'started'], ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'sprint',
    textField: 'sprintName',
    valueField: 'sprintId',
    request: () => sprintApi.loadSprints(statusList),
    paging: false,
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
      optionRenderer={({ record, text, value }) => (
        <Tooltip title={text}>
          <span>{text}</span>
        </Tooltip>
      )}
    />
  );
});
export default SelectSprint;
