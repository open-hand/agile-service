import React, { useMemo } from 'react';
import { Select, CheckBox } from 'choerodon-ui/pro';
import { find } from 'lodash';
import { IQuickSearchValue } from '@/components/quick-search';
import SelectSprint from '@/components/select/select-sprint';
import IssueSearch, { IssueSearchStore } from '@/components/issue-search';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import { ISearchVO, ISprint } from '@/common/types';
import { IBurndownChartType } from '.';

const { Option } = Select;

export interface BurnDownSearchProps {
  projectId?: string
  sprintId: string | undefined
  setSprintId: (sprintId: string | undefined) => void
  currentSprintId: string | undefined
  setCurrentSprintId: (sprintId: string | undefined) => void
  setEndDate: (endDate: string) => void
  type: IBurndownChartType
  setType: (type: IBurndownChartType) => void
  quickFilter: IQuickSearchValue
  setQuickFilter: (quickFilter: IQuickSearchValue) => void
  restDayShow: boolean
  setRestDayShow: (restDayShow: boolean) => void
  useCurrentSprint?: boolean
  setUseCurrentSprint: (useCurrentSprint: boolean) => void
  searchVO?: ISearchVO
  setSearchVO: (searchVO: ISearchVO) => void
  onEmpty: () => void
}
const BurndownSearch: React.FC<BurnDownSearchProps> = ({
  projectId,
  sprintId,
  setSprintId,
  currentSprintId,
  setCurrentSprintId,
  setEndDate,
  type,
  setType,
  quickFilter,
  setQuickFilter,
  restDayShow,
  setRestDayShow,
  useCurrentSprint,
  setUseCurrentSprint,
  searchVO,
  setSearchVO,
  onEmpty,
}) => {
  const issueSearchStore = useMemo(() => new IssueSearchStore({
    transformFilter,
    defaultSearchVO: searchVO,
    // @ts-ignore
    getSystemFields: () => getSystemFields().filter((f) => !['contents', 'sprint', 'quickFilterIds'].includes(f.code)),
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }), []);

  return (
    <div>
      <SelectSprint
        label="迭代冲刺"
        labelLayout={'float' as LabelLayout}
        clearButton={false}
        projectId={projectId}
        statusList={['started', 'closed']}
        currentSprintOption
        afterLoad={(sprints) => {
          if (useCurrentSprint && !currentSprintId) {
            const current = find(sprints, { statusCode: 'started' });
            if (current) {
              setSprintId(current.sprintId);
              setCurrentSprintId(current.sprintId);
            } else {
              setSprintId(undefined);
              setCurrentSprintId(undefined);
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
      <CheckBox
        style={{ marginLeft: 24 }}
        checked={restDayShow}
        onChange={setRestDayShow}
      >
        显示非工作日
      </CheckBox>
      <IssueSearch
        store={issueSearchStore}
        onClear={() => {
          const newSearchVO = issueSearchStore.getCustomFieldFilters();
          setSearchVO(newSearchVO);
        }}
        onChange={() => {
          const newSearchVO = issueSearchStore.getCustomFieldFilters();
          setSearchVO(newSearchVO);
        }}
      />

    </div>
  );
};
export default BurndownSearch;
