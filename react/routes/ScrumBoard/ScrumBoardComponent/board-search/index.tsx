import React, { useEffect, useState } from 'react';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import IssueSearch, { IssueSearchStore, useIssueSearchStore } from '@/components/issue-search';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { transformFilter } from '@/routes/Issue/stores/utils';
import openSaveFilterModal from '@/components/SaveFilterModal';
import { Tooltip } from 'choerodon-ui';
import scrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import './index.less';
import FilterManage from '@/components/FilterManage';

interface Props {
    onRefresh: () => void
    saveStore: (store: IssueSearchStore) => void
    //   issueSearchStore: IssueSearchStore,
}

const BoardSearch: React.FC<Props> = ({ onRefresh, saveStore }) => {
  const [visible, setVisible] = useState<boolean>(false);

  const issueSearchStore = useIssueSearchStore({
    // @ts-ignore
    getSystemFields: () => getSystemFields(['issueTypeId']),
    transformFilter,
    // @ts-ignore
    defaultChosenFields: Array.isArray(localPageCacheStore.getItem('scrumBoard.search')) ? new Map(localPageCacheStore.getItem('scrumBoard.search').map((item) => [item.code, item])) : undefined,
  });
  const handleClear = () => {
    scrumBoardStore.clearFilter();
    localPageCacheStore.remove('scrumBoard.search');
    localPageCacheStore.remove('scrumBoard.searchVO');
    onRefresh();
    // StoryMapStore.clearData();
    // StoryMapStore.getStoryMap();
  };
  useEffect(() => {
    saveStore(issueSearchStore);
  }, [issueSearchStore, saveStore]);
  const handleClickSaveFilter = () => {
    openSaveFilterModal({ searchVO: issueSearchStore.getCustomFieldFilters(), onOk: issueSearchStore.loadMyFilterList });
  };

  return (
    <div className="c7n-agile-scrum-board-search" style={{ display: 'flex' }}>
      <IssueSearch
        store={issueSearchStore}
        onClear={handleClear}
        onClickSaveFilter={handleClickSaveFilter}
        onChange={() => {
          const newSearch = issueSearchStore.getCustomFieldFilters();
          localPageCacheStore.setItem('scrumBoard.search', issueSearchStore.currentFilter);
          localPageCacheStore.setItem('scrumBoard.searchVO', newSearch);
          //   ScrumBoardStore
          scrumBoardStore.setSearchVO(newSearch);
          onRefresh();
          //   StoryMapStore.setSearchVO(issueSearchStore.getCustomFieldFilters());
          //   StoryMapStore.clearData();
          //   StoryMapStore.getStoryMap();
        }}
      />

    </div>

  );
};

export default observer(BoardSearch);
