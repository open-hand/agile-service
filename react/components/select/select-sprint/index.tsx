import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { Tooltip } from 'choerodon-ui';
import { sprintApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ISprint } from '@/common/types';

interface Props extends Partial<SelectProps> {
  isProgram?: boolean,
  statusList?: string[],
  selectSprints?: number[],
  afterLoad?: (sprints: ISprint[]) => void
  addExtraOptions?: (sprints: ISprint[]) => ISprint[]
  projectId?: string
  currentSprintOption?: boolean
  dataRef?: React.MutableRefObject<any>
}

const SelectSprint: React.FC<Props> = forwardRef(({
  statusList = ['sprint_planning', 'started'],
  isProgram,
  selectSprints,
  afterLoad,
  addExtraOptions,
  projectId,
  currentSprintOption,
  dataRef,
  ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Function>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig<ISprint> => ({
    name: 'sprint',
    textField: 'sprintName',
    valueField: 'sprintId',
    request: ({ filter, page }) => (isProgram ? sprintApi.loadSubProjectSprints(filter || '', page!, selectSprints)
      : sprintApi.project(projectId).loadSprints(statusList)),
    middleWare: (sprints) => {
      if (afterLoadRef.current) {
        afterLoadRef.current(sprints);
      }
      let newSprint = sprints;
      if (addExtraOptions) {
        newSprint = addExtraOptions(sprints);
      }
      if (currentSprintOption) {
        newSprint = [{ sprintId: 'current', sprintName: '当前冲刺' } as ISprint, ...newSprint];
      }
      if (dataRef) {
        Object.assign(dataRef, {
          current: newSprint,
        });
      }
      return newSprint;
    },
    paging: !!isProgram,
  }), [isProgram, projectId, selectSprints, JSON.stringify(statusList), currentSprintOption]);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
      // @ts-ignore
      optionRenderer={({ record, text, value }) => (
        <Tooltip title={text}>
          <span>{text}</span>
        </Tooltip>
      )}
    />
  );
});
export default SelectSprint;
