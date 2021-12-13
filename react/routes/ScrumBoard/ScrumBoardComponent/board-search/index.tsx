import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { useUnmount } from 'ahooks';
import { sortBy } from 'lodash';
import IssueSearch, { IssueSearchStore, useIssueSearchStore } from '@/components/issue-search';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getSystemFields as originGetSystemFields } from '@/stores/project/issue/IssueStore';
import { transformFilter as originTransformFilter } from '@/routes/Issue/stores/utils';
import openSaveFilterModal from '@/components/SaveFilterModal';
import scrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import { IChosenFields, ILocalField } from '@/components/issue-search/store';
import SelectSprint from '@/components/select/select-sprint';
import './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

interface Props {
  onRefresh: () => void
  saveStore: (store: IssueSearchStore) => void
  excludeQuickFilterIds?: string[]
  //   issueSearchStore: IssueSearchStore,
}
function getSystemFields() {
  const preSystemFieldCodes = ['quickFilterIds', 'issueIds', 'sprint', 'assigneeId', 'priorityId'];
  const preSystemFieldCodeSequence = preSystemFieldCodes.reduce((p, code, index) => ({ ...p, [code]: index }), {});

  const preSystemFields = sortBy(originGetSystemFields()
    .filter((field) => preSystemFieldCodes.includes(field.code)).map((item) => {
      if (item.code === 'sprint') {
        return { ...item, fieldType: 'single' };
      }
      return { ...item, defaultShow: true };
    }), (field) => preSystemFieldCodeSequence[field.code as keyof typeof preSystemFieldCodeSequence]);
  const systemFields = originGetSystemFields(preSystemFieldCodes) as any;

  systemFields.unshift(...preSystemFields);
  return systemFields;
}
const ObserverSelectSprint: React.FC<any> = observer(({ field, value, ...otherProps }) => {
  const formatMessage = useFormatMessage();
  return (
    <SelectSprint
      key={field.code}
      value={Array.isArray(value) ? value[0] : value || null} // 缓存内的数据有可能是数组
      flat
      placeholder={formatMessage({ id: field.nameKey as any })}
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
  );
});
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
const fieldConfigs = {
  issueTypeId: { excludeTypeCodes: ['issue_epic'] },
};
const BoardSearch: React.FC<Props> = ({ onRefresh, saveStore, excludeQuickFilterIds }) => {
  const issueSearchStore = useIssueSearchStore({
    // @ts-ignore
    getSystemFields,
    transformFilter,
    renderField,
    fieldConfigs,
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
    openSaveFilterModal({
      searchVO: issueSearchStore.getCustomFieldFilters(),
      onOk: issueSearchStore.loadMyFilterList,
      hiddenDefault: true,
    });
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
        excludeQuickFilterIds={excludeQuickFilterIds}
      />
    </div>

  );
};

BoardSearch.defaultProps = {
  excludeQuickFilterIds: undefined,
};

export default observer(BoardSearch);
