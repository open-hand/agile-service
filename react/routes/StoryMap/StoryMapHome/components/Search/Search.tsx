import React from 'react';
import { Button, Tooltip } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { useUnmount } from 'ahooks';
import IssueSearch, { IssueSearchStore } from '@/components/issue-search';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import openSaveFilterModal from '@/components/SaveFilterModal';

import styles from './Search.less';
import useUnmountSaveCache from '@/hooks/useUnmountSaveCache';

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
  useUnmountSaveCache('storyMapSearchVO', () => issueSearchStore.getCustomFieldFilters(true));
  return (
    <div className={styles.storyMapSearch}>
      <IssueSearch
        store={issueSearchStore}
        onClear={handleClear}
        onChange={() => {
          StoryMapStore.setSearchVO(issueSearchStore.getCustomFieldFilters());
          StoryMapStore.clearData();
          StoryMapStore.getStoryMap();
        }}
        foldedHeight={42}
      // onClickSaveFilter={handleClickSaveFilter}
      />
      <div style={{
        marginLeft: 'auto',
      }}
      >
        <Tooltip title="故事地图默认显示最近5个冲刺/版本，您可以使用筛选选择其他或者更多的冲刺/版本。" placement="bottomLeft">
          <Button
            icon="help"
            style={{
              color: '#5365EA',
              fontWeight: 500,
              borderRadius: '6px',
              margin: '14px 15px 0px 2px',
              height: 30,
              flexShrink: 0,
              background: 'transparent',
            }}
          />
        </Tooltip>
      </div>

    </div>

  );
};

export default observer(StoryMapSearch);
