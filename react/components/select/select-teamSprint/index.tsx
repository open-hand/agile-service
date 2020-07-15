import React, { useState, useEffect, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { sprintApi } from '@/api';

const { OptGroup, Option } = Select;
interface Props {
  teamIds: number[],
  piId: number
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
const SelectSprint: React.FC<Props> = forwardRef(({ teamIds, piId, ...otherProps }, ref: React.Ref<Select>) => {
  const [teams, setTeams] = useState<Team[]>([]);
  const loadData = async () => {
    if (piId && Array.isArray(teamIds) && teamIds.length > 0) {
      const res = await sprintApi.getTeamSprints(piId, teamIds);
      setTeams(res);
    } else {
      setTeams([]);
    }
  };
  useEffect(() => {
    loadData();
  }, [piId, JSON.stringify(teamIds)]);
  return (
    <Select
      ref={ref}
      multiple
      {...otherProps}
    >
      {
        teams.map((team: Team) => (
          <OptGroup label={team.projectVO.name} key={team.projectVO.id}>
            {(team.sprints || []).map((sprint: Sprint) => (
              <Option key={`${sprint.sprintId}`} value={sprint.sprintId}>
                {sprint.sprintName}
              </Option>
            ))}
          </OptGroup>
        ))
      }
    </Select>
  );
});
export default SelectSprint;
