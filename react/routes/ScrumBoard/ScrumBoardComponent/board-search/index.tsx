import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { useUnmount } from 'ahooks';
import IssueSearch, { IssueSearchStore, useIssueSearchStore } from '@/components/issue-search';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getSystemFields as originGetSystemFields } from '@/stores/project/issue/IssueStore';
import { transformFilter as originTransformFilter } from '@/routes/Issue/stores/utils';
import openSaveFilterModal from '@/components/SaveFilterModal';
import scrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import { IChosenFields, ILocalField } from '@/components/issue-search/store';
import SelectSprint from '@/components/select/select-sprint';
import './index.less';

interface Props {
  onRefresh: () => void
  saveStore: (store: IssueSearchStore) => void
  //   issueSearchStore: IssueSearchStore,
}
function getSystemFields() {
  const systemFields = originGetSystemFields(['quickFilterIds', 'issueIds', 'sprint', 'assigneeId', 'priorityId']);
  systemFields.unshift(...[{
    code: 'quickFilterIds',
    name: '快速筛选',
    defaultShow: true,
    noDisplay: true,
  }, {
    code: 'assigneeId',
    name: '经办人',
    defaultShow: true,
    fieldType: 'member',
  }, {
    code: 'sprint',
    name: '冲刺',
    defaultShow: true,
    fieldType: 'single',
  }, {
    code: 'priorityId',
    name: '优先级',
    defaultShow: true,
    fieldType: 'multiple',
  },
  ]);
  return systemFields;
}
const ObserverSelectSprint: React.FC<any> = observer(({ field, value, ...otherProps }) => (
  <SelectSprint
    key={field.code}
    value={Array.isArray(value) ? value[0] : value || null} // 缓存内的数据有可能是数组
    flat
    placeholder={field.name}
    maxTagTextLength={10}
    afterLoad={(sprints) => {
      scrumBoardStore.setSprintNotClosedArray(sprints);
      const startedSprint = sprints.find((sprint) => sprint.statusCode === 'started');
      if (startedSprint) {
        scrumBoardStore.executeBindFunction(['refresh'], startedSprint.sprintId);
      } else {
        scrumBoardStore.executeBindFunction(['refresh']);
      }
      scrumBoardStore.removeBindFunction('refresh');
    }}
    dropdownMatchSelectWidth={false}
    clearButton
    optionRenderer={({ record, text }) => {
      if (record?.get('statusCode') === 'started') {
        return (
          <span>
            {text}
            <div className="c7n-agile-sprintSearchSelect-option-active">活跃</div>
          </span>
        );
      }
      return <span>{text}</span>;
    }}
    {...otherProps}
  />
));
function renderField(field: ILocalField, props: any) {
  if (field.code === 'sprint') {
    return <ObserverSelectSprint field={field} {...props} />;
  }
  return null;
}
function transformFilter(chosenFields: IChosenFields) {
  const filter = originTransformFilter(chosenFields);
  if (filter.otherArgs.sprint && filter.otherArgs.sprint !== '' && !Array.isArray(filter.otherArgs.sprint)) {
    filter.otherArgs.sprint = [filter.otherArgs.sprint];
  }
  return filter;
}
const BoardSearch: React.FC<Props> = ({ onRefresh, saveStore }) => {
  const issueSearchStore = useIssueSearchStore({
    // @ts-ignore
    getSystemFields,
    transformFilter,
    renderField,
    defaultSearchVO: localPageCacheStore.getItem('scrumBoard.searchVO'),
  });
  const handleClear = () => {
    scrumBoardStore.clearFilter();
    localPageCacheStore.remove('scrumBoard.searchVO');
    onRefresh();
    // StoryMapStore.clearData();
    // StoryMapStore.getStoryMap();
  };
  useEffect(() => {

  }, [issueSearchStore.isHasFilter]);
  useEffect(() => {
    saveStore(issueSearchStore);
  }, [issueSearchStore, saveStore]);
  const handleClickSaveFilter = () => {
    openSaveFilterModal({ searchVO: issueSearchStore.getCustomFieldFilters(), onOk: issueSearchStore.loadMyFilterList });
  };
  useUnmount(() => { localPageCacheStore.setItem('scrumBoard.searchVO', issueSearchStore.getCustomFieldFilters(true)); });
  return (
    <div className="c7n-agile-scrum-board-search" style={{ display: 'flex' }}>
      <IssueSearch
        store={issueSearchStore}
        onClear={handleClear}
        onClickSaveFilter={handleClickSaveFilter}
        onChange={() => {
          const newSearch = issueSearchStore.getCustomFieldFilters();
          scrumBoardStore.setSearchVO(newSearch);
          onRefresh();
        }}
        foldedHeight={40}
      />
    </div>

  );
};

export default observer(BoardSearch);
