import React, {
  useState, useEffect, forwardRef, useCallback, useMemo, useRef,
} from 'react';
import { find, omit, pick } from 'lodash';
import { Select, Tooltip } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { useClickAway } from 'ahooks';
import { sprintApi } from '@/api';

const { OptGroup, Option } = Select;
interface Props extends Partial<SelectProps> {
  teamIds: number[],
  piId: number
  hasUnassign?: boolean
  afterLoad?: (data: any[]) => void
  flat?: boolean
  dataRef?: React.MutableRefObject<any>
}
interface Sprint {
  sprintId: number,
  sprintName: string
}
interface Team {
  projectVO: {
    name: string,
    id: number,
  },
  sprints: Sprint[]
}
const SelectSprintDisabled: React.FC<{ tooltipTitle: string } & Partial<SelectProps>> = ({ tooltipTitle, onBlur, ...otherProps }) => {
  const ref = useRef<any>();
  const firstTriggerClickAway = useRef<boolean>(true);
  useClickAway((e) => {
    if (onBlur) {
      !firstTriggerClickAway.current && onBlur(e as any);
      firstTriggerClickAway.current = !firstTriggerClickAway.current;
    }
  }, ref);
  return (
    <Tooltip title={tooltipTitle}>
      <div
        ref={ref}

      >
        <Select {...otherProps} disabled />
      </div>
    </Tooltip>
  );
};
const SelectSprint: React.FC<Props> = forwardRef(({
  teamIds, piId, hasUnassign, afterLoad, flat, dataRef,
  ...otherProps
}, ref: React.Ref<Select>) => {
  const [teams, setTeams] = useState<Team[]>([]);
  const sprints = useMemo(() => teams.reduce((result, team) => [
    ...result,
    ...(team.sprints || []),
  ], []), [teams]);
  if (dataRef) {
    Object.assign(dataRef, {
      current: sprints,
    });
  }
  const loadData = useCallback(async () => {
    if (piId && Array.isArray(teamIds) && teamIds.length > 0) {
      const res = await sprintApi.getTeamSprints(piId, teamIds);
      const newRes = res.map((item: Team) => (hasUnassign ? {
        ...item,
        sprints: [{ sprintId: `${item.projectVO.id}**0`, sprintName: '未分配冲刺', projectId: item.projectVO.id }, ...item.sprints],
      } : item));
      setTeams(newRes);
      if (afterLoad) {
        afterLoad(newRes);
      }
    } else {
      setTeams([]);
      if (afterLoad) {
        afterLoad([]);
      }
    }
  }, [piId, JSON.stringify(teamIds)]);
  useEffect(() => {
    loadData();
  }, [loadData]);
  if (!(piId && teamIds?.length)) {
    const lackParams = [!piId && 'PI', !teamIds.length && '负责的子项目'].filter(Boolean) as string[];
    return <SelectSprintDisabled tooltipTitle={`请先选择${lackParams.join('和')}`} {...otherProps} />;
  }
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      multiple
      // @ts-ignore
      renderer={({ value }) => find(sprints, { sprintId: value })?.sprintName}
      {...otherProps}
    >
      {
        teams.map((team: Team) => (
          <OptGroup label={team.projectVO.name} key={team.projectVO.id}>
            {(team.sprints || []).map((sprint: Sprint) => (
              <Option key={`${sprint.sprintId}`} value={sprint.sprintId}>
                <Tooltip title={sprint.sprintName}>
                  {sprint.sprintName}
                </Tooltip>
              </Option>
            ))}
          </OptGroup>
        ))
      }
    </Component>
  );
});
export default SelectSprint;
