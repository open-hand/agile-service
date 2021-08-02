import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { Tooltip } from 'choerodon-ui/pro';
import { sprintApi } from '@/api';
import useSelect, { SelectConfig, FragmentForSearch } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ISprint } from '@/common/types';
import { FlatSelect } from '@choerodon/components';
import './index.less';

interface Props extends Partial<SelectProps> {
  hasUnassign?: boolean,
  isProgram?: boolean,
  statusList?: string[],
  selectSprints?: number[],
  afterLoad?: (sprints: ISprint[]) => void
  projectId?: string
  currentSprintOption?: boolean
  dataRef?: React.MutableRefObject<any>
  flat?: boolean
}

const SelectSprint: React.FC<Props> = forwardRef(({
  statusList = ['sprint_planning', 'started'],
  isProgram,
  hasUnassign,
  selectSprints,
  afterLoad,
  projectId,
  currentSprintOption,
  dataRef,
  flat,
  ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<ISprint> => ({
    name: 'sprint',
    textField: 'sprintName',
    valueField: 'sprintId',
    afterLoad,
    optionRenderer: (sprint) => (
      <FragmentForSearch name={sprint.sprintName}>
        <div style={{ display: 'inline-block' }}>
          {sprint.sprintName}
          {sprint.statusCode === 'started' && <div className="c7n-agile-sprintSearchSelect-option-active">活跃</div>}
        </div>
      </FragmentForSearch>
    ),
    request: ({ filter, page }) => (isProgram ? sprintApi.loadSubProjectSprints(filter || '', page!, selectSprints)
      : sprintApi.project(projectId).loadSprints(statusList)),
    middleWare: (sprints) => {
      let newSprint = sprints;
      if (hasUnassign) {
        newSprint = [{ sprintId: '0', sprintName: '未分配冲刺', endDate: '' } as ISprint, ...sprints];
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
  }), [isProgram, projectId, selectSprints, JSON.stringify(statusList), currentSprintOption, afterLoad]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      {...props}
      {...otherProps}
      // @ts-ignore
      // optionRenderer={({ record, text, value }) => (
      //   <Tooltip title={text}>
      //     <span>{text}</span>
      //   </Tooltip>
      // )}
    />
  );
});
export default SelectSprint;
