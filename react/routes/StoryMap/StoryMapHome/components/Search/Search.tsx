import React from 'react';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import IssueSearch, { IssueSearchStore } from '@/components/issue-search';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import openSaveFilterModal from '@/components/SaveFilterModal';
import { Tooltip } from 'choerodon-ui';
import styles from './Search.less';

interface Props {
  issueSearchStore: IssueSearchStore,
}

const StoryMapSearch: React.FC<Props> = ({ issueSearchStore }) => {
  const handleClear = () => {
    StoryMapStore.clearData();
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
          StoryMapStore.clearData();
          StoryMapStore.getStoryMap();
        }}
        // onClickSaveFilter={handleClickSaveFilter}
      />
      <Tooltip title="故事地图默认显示最近5个冲刺/版本，您可以使用筛选选择其他或者更多的冲刺/版本。" placement="bottomLeft">
        <Button
          icon="help"
          style={{
            color: '#5365EA',
            border: '1px solid #D9E6F2',
            fontWeight: 500,
            borderRadius: '6px',
            margin: '8px 15px 0px 10px',
            flexShrink: 0,
          }}
        />
      </Tooltip>
    </div>

  );
};

export default observer(StoryMapSearch);
