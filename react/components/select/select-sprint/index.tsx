import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';

import type { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import classNames from 'classnames';
import { sprintApi } from '@/api';
import useSelect, { SelectConfig, FragmentForSearch } from '@/hooks/useSelect';
import { ISprint } from '@/common/types';
import renderEllipsisBlockOption, { styles } from '../select-pi/utils';

export interface SelectSprintProps extends Partial<SelectProps> {
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

const SelectSprint: React.FC<SelectSprintProps> = forwardRef(({
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
    tooltip: false,
    afterLoad,
    optionRenderer: (sprint) => (
      <FragmentForSearch name={sprint.sprintName}>
        {renderEllipsisBlockOption(sprint.sprintName, <>活跃</>, { showBlock: sprint.statusCode === 'started', tooltip: true })}
      </FragmentForSearch>
    ),
    renderer: (sprint) => renderEllipsisBlockOption(sprint.sprintName, <>活跃</>, { showBlock: sprint.statusCode === 'started', tooltip: false }) as JSX.Element,
    request: ({ filter, page }) => (isProgram ? sprintApi.loadSubProjectSprints(filter || '', page!, selectSprints, 50)
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
    props: {
      onOption: () => ({ className: styles.option }),
    },
    paging: !!isProgram,
  }), [isProgram, projectId, selectSprints, JSON.stringify(statusList), currentSprintOption, afterLoad]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      dropdownMatchSelectWidth={false}
      popupStyle={{ minWidth: '2rem' }}
      {...props}
      {...otherProps}
      popupCls={classNames(styles.popup, otherProps.popupCls)}
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
