import React from 'react';
import { Select, CheckBox } from 'choerodon-ui/pro';
import QuickSearch, { IQuickSearchValue } from '@/components/quick-search';
import SelectSprint from '@/components/select/select-sprint';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import { ISprint } from '@/common/types';
import { IBurndownChartType } from '.';

const { Option } = Select;

export interface BurnDownSearchProps {
  sprintId: string | undefined
  setSprintId: (sprintId: string | undefined) => void
  setEndDate: (endDate: string) => void
  type: IBurndownChartType
  setType: (type: IBurndownChartType) => void
  quickFilter: IQuickSearchValue
  setQuickFilter: (quickFilter: IQuickSearchValue) => void
  restDayShow: boolean
  setRestDayShow: (restDayShow: boolean) => void
}
const BurndownSearch: React.FC<BurnDownSearchProps> = ({
  sprintId,
  setSprintId,
  setEndDate,
  type,
  setType,
  quickFilter,
  setQuickFilter,
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
    <Select
      clearButton={false}
      labelLayout={'float' as LabelLayout}
      style={{ width: 244, marginLeft: 24 }}
      label="单位"
      value={type}
      onChange={setType}
    >
      <Option value="remainingEstimatedTime">剩余时间</Option>
      <Option value="storyPoints">故事点</Option>
      <Option value="issueCount">问题计数</Option>
    </Select>
    <QuickSearch
      style={{ marginLeft: 24, width: 244 }}
      onChange={setQuickFilter}
      value={quickFilter}
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
export default BurndownSearch;
