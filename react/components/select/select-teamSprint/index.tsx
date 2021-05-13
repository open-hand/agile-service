import React, {
  useState, useEffect, forwardRef, useCallback, useMemo,
} from 'react';
import { find } from 'lodash';
import { Select } from 'choerodon-ui/pro';
import { sprintApi } from '@/api';
import { Tooltip } from 'choerodon-ui';
import { FlatSelect } from '@choerodon/components';

const { OptGroup, Option } = Select;
interface Props {
  teamIds: number[],
  piId: number
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
const SelectSprint: React.FC<Props> = forwardRef(({
  teamIds, piId, afterLoad, flat, dataRef,
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
      setTeams(res);
      if (afterLoad) {
        afterLoad(res);
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
