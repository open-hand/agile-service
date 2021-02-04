import React from 'react';
import { CheckBox } from 'choerodon-ui/pro';
import { find } from 'lodash';
import SelectSprint from '@/components/select/select-sprint';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import { ISprint } from '@/common/types';

export interface SprintSearchProps {
  sprintId: string | undefined
  setSprintId: (sprintId: string | undefined) => void
  currentSprintId: string | undefined
  setCurrentSprintId: (sprintId: string | undefined) => void
  setEndDate: (endDate: string) => void
  restDayShow: boolean
  setRestDayShow: (restDayShow: boolean) => void
  useCurrentSprint?: boolean
  setUseCurrentSprint: (useCurrentSprint: boolean) => void
  projectId?:string
  onEmpty: () => void
}
const SprintSearch: React.FC<SprintSearchProps> = ({
  sprintId,
  setSprintId,
  currentSprintId,
  setCurrentSprintId,
  setEndDate,
  restDayShow,
  setRestDayShow,
  useCurrentSprint,
  setUseCurrentSprint,
  projectId,
  onEmpty,
}) => (
  <div>
    <SelectSprint
      label="迭代冲刺"
      labelLayout={'float' as LabelLayout}
      clearButton={false}
      projectId={projectId}
      statusList={['started', 'closed']}
      currentSprintOption
        // onOption={({ record }) => ({
        //   disabled: record.get('sprintId') === '0' && !currentSprintId,
        // })}
      afterLoad={(sprints) => {
        const current = find(sprints, { statusCode: 'started' });
        if (current) {
          setCurrentSprintId(current.sprintId);
        }
        if (useCurrentSprint && !currentSprintId) {
          if (current) {
            setSprintId(current.sprintId);
          } else {
            setSprintId(undefined);
            onEmpty();
          }
        } else if (!sprintId && sprints.length > 0) {
          setSprintId(sprints[0].sprintId);
        }
      }}
      value={useCurrentSprint ? 'current' : sprintId}
      primitiveValue={false}
      onChange={(sprint: ISprint | null) => {
        if (sprint && sprint.sprintId === 'current') {
          setSprintId(currentSprintId);
          setUseCurrentSprint(true);
          return;
        }
        if (sprint) {
          setSprintId(sprint.sprintId);
          setEndDate(sprint.endDate);
          setUseCurrentSprint(false);
        } else {
          setSprintId(undefined);
          setEndDate('');
          setUseCurrentSprint(false);
        }
      }}
    />
    <CheckBox
      style={{ marginLeft: 24 }}
      checked={restDayShow}
      onChange={setRestDayShow}
    >
      显示非工作日
    </CheckBox>
  </div>
);
export default SprintSearch;
