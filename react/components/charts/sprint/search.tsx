import React from 'react';
import { CheckBox } from 'choerodon-ui/pro';
import SelectSprint from '@/components/select/select-sprint';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import { ISprint } from '@/common/types';

export interface SprintSearchProps {
  sprintId: string | undefined
  setSprintId: (sprintId: string | undefined) => void
  setEndDate: (endDate: string) => void
  restDayShow: boolean
  setRestDayShow: (restDayShow: boolean) => void
}
const SprintSearch: React.FC<SprintSearchProps> = ({
  sprintId,
  setSprintId,
  setEndDate,
  restDayShow,
  setRestDayShow,
}) => (
  <div>
    <SelectSprint
      label="迭代冲刺"
      labelLayout={'float' as LabelLayout}
      clearButton={false}

      statusList={['started', 'closed']}
      afterLoad={(sprints) => {
        if (!sprintId && sprints.length > 0) {
          setSprintId(sprints[0].sprintId);
        }
      }}
      value={sprintId}
      primitiveValue={false}
      onChange={(sprint: ISprint | null) => {
        if (sprint) {
          setSprintId(sprint.sprintId);
          setEndDate(sprint.endDate);
        } else {
          setSprintId(undefined);
          setEndDate('');
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
