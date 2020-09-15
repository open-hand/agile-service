import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { Tooltip } from 'choerodon-ui';
import { sprintApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ISprint } from '@/common/types';

interface Props extends Partial<SelectProps> {
  statusList?: string[]
  afterLoad?: (sprints: ISprint[]) => void
}

const SelectSprint: React.FC<Props> = forwardRef(({
  statusList = ['sprint_planning', 'started'],
  afterLoad, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<ISprint> => ({
    name: 'sprint',
    textField: 'sprintName',
    valueField: 'sprintId',
    request: () => sprintApi.loadSprints(statusList),
    middleWare: (sprints) => {
      if (afterLoad) {
        afterLoad(sprints);
      }
      return sprints;
    },
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
