import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import IssueSearch, { useIssueSearchStore } from '@/components/issue-search';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import styles from './Search.less';

const StoryMapSearch = () => {
  const issueSearchStore = useIssueSearchStore({
    // @ts-ignore
    getSystemFields,
    transformFilter,
    // @ts-ignore
    defaultChosenFields: Array.isArray(localPageCacheStore.getItem('storyMapFilter')) ? new Map(localPageCacheStore.getItem('storyMapFilter').map((item) => [item.code, item])) : undefined,
  });

  const handleClear = () => {
    StoryMapStore.getStoryMap();
  };

  const handleClickSaveFilter = useCallback(() => {
    console.log('保存筛选');
    // StoryMapStore.setSaveFilterVisible(true);
    // StoryMapStore.setFilterListVisible(false);
  }, []);

  return (
    <div className={styles.storyMapSearch}>
      <IssueSearch
        store={issueSearchStore}
        onClear={handleClear}
        onChange={() => {
          localPageCacheStore.setItem('storyMapFilter', issueSearchStore.currentFilter);
          StoryMapStore.getStoryMap();
        }}
        onClickSaveFilter={handleClickSaveFilter}
      />
    </div>

  );
};

export default observer(StoryMapSearch);
