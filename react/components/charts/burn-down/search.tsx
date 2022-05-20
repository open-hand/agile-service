import React, {
  MutableRefObject, useMemo, useRef, useImperativeHandle, useEffect,
} from 'react';
import { Select, CheckBox, DataSet } from 'choerodon-ui/pro';
import { find } from 'lodash';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import { IQuickSearchValue } from '@/components/quick-search';
import SelectSprint from '@/components/select/select-sprint';
import IssueSearch, { IssueSearchStore } from '@/components/issue-search';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
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
  /**
   * 当前图表搜索 的DataSet
   */
   searchDataSet?:DataSet
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
  searchDataSet,
}) => {
  const sprintsRef = useRef<ISprint[]>([]);
  const issueSearchStore = useMemo(() => new IssueSearchStore({
    projectId,
    transformFilter,
    defaultSearchVO: searchVO,
    // @ts-ignore
    getSystemFields: () => getSystemFields().filter((f) => !['contents', 'sprint', 'quickFilterIds'].includes(f.code)),
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }), []);
  return (
    <div>
      <SelectSprint
        style={{ width: 280 }}
        label="迭代冲刺"
        dataSet={searchDataSet}
        name="sprint"
        labelLayout={'float' as LabelLayout}
        clearButton={false}
        projectId={projectId}
        statusList={['started', 'closed']}
        currentSprintOption
        // onOption={({ record }) => ({
        //   disabled: record.get('sprintId') === '0' && !currentSprintId,
        // })}
        afterLoad={(sprints) => {
          sprintsRef.current = sprints;
          const current = find(sprints, { statusCode: 'started' });
          if (current) {
            setCurrentSprintId(current.sprintId);
          }
          if (useCurrentSprint && !currentSprintId) {
            if (current) {
              setSprintId(current.sprintId);
              setEndDate(current.endDate);
            } else {
              setSprintId(undefined);
              setEndDate('');
              onEmpty();
            }
          } else if (sprints.length > 0) {
            if (!sprintId) {
              setSprintId(sprints[0].sprintId);
              setEndDate(sprints[0].endDate);
            } else {
              const target = find(sprints, { sprintId });
              if (target) {
                setEndDate(target.endDate);
              }
            }
          }
        }}
        value={useCurrentSprint ? 'current' : sprintId}
        primitiveValue={false}
        onChange={(sprint: ISprint | null) => {
          if (sprint && sprint.sprintId === 'current') {
            setSprintId(currentSprintId);
            const target = find(sprintsRef.current, { sprintId });
            if (target) {
              setEndDate(target.endDate);
            }
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
        dataSet={searchDataSet}
        labelLayout={'float' as LabelLayout}
        style={{ width: 200, marginLeft: 20 }}
        label="单位"
        name="unit"
        value={type}
        onChange={setType}
      >
        <Option value="remainingEstimatedTime">剩余时间</Option>
        <Option value="storyPoints">故事点</Option>
        <Option value="issueCount">工作项计数</Option>
      </Select>
      <CheckBox
        style={{ marginLeft: 24 }}
        checked={restDayShow}
        onChange={setRestDayShow}
      >
        显示非工作日
      </CheckBox>
      <div style={{ marginLeft: -5, marginTop: 15 }}>
        <IssueSearch
          projectId={projectId}
          applyType="agile"
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
    </div>
  );
};
export default BurndownSearch;
