import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import IssueSearch, { IssueSearchStore } from '@/components/issue-search';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import openSaveFilterModal from '@/components/SaveFilterModal';
import styles from './Search.less';

interface Props {
  issueSearchStore: IssueSearchStore,
}

const StoryMapSearch: React.FC<Props> = ({ issueSearchStore }) => {
  const handleClear = () => {
    StoryMapStore.getStoryMap();
  };

  const handleClickSaveFilter = () => {
    openSaveFilterModal({ searchVO: issueSearchStore.getCustomFieldFilters(), onOk: issueSearchStore.loadMyFilterList });
  };

  return (
    <div className={styles.storyMapSearch}>
      <IssueSearch
        store={issueSearchStore}
        onClear={handleClear}
        onChange={() => {
          localPageCacheStore.setItem('storyMapFilter', issueSearchStore.currentFilter);
          localPageCacheStore.setItem('storyMapSearchVO', issueSearchStore.getCustomFieldFilters());
          StoryMapStore.setSearchVO(issueSearchStore.getCustomFieldFilters());
          StoryMapStore.getStoryMap();
        }}
        onClickSaveFilter={handleClickSaveFilter}
      />
    </div>

  );
};

export default observer(StoryMapSearch);
