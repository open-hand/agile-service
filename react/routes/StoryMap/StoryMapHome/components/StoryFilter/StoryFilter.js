import React from 'react';
import {
  Select, Form, Button,
} from 'choerodon-ui/pro';
import { Divider } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';
import './index.less';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';

const StoryFilter = ({ selectDataSet, hasFilter }) => {
  const handleResetFilter = () => {
    localPageCacheStore.remove('stroyMap.filter');
    selectDataSet.reset();
    StoryMapStore.resetSearchVO();
    StoryMapStore.getStoryMap();
  };

  return (
    <div className="c7nagile-StoryMap-storyFilter">
      <Form>
        <Select
          className="c7nagile-StoryMap-storyFilter-select c7nagile-StoryMap-storyFilter-isCompletedSelect"
          dataSet={selectDataSet}
          name="isCompleted"
          placeholder="解决状态"
        />
        <Select
          className="c7nagile-StoryMap-storyFilter-select c7nagile-StoryMap-storyFilter-sprintSelect"
          dataSet={selectDataSet}
          name="sprints"
          placeholder="冲刺"
          multiple
          searchable
          dropdownMatchSelectWidth={false}
          maxTagCount={2}
          maxTagTextLength={8}
          maxTagPlaceholder={(restValues) => `+${restValues.length}...`}
          optionRenderer={({ record, text }) => (
            <div style={{ display: 'inline-block' }}>
              {text}
              {
                    record.get('statusCode') === 'started' && (
                    <div className="c7nagile-StoryMap-storyFilter-sprintSelect-option-active">活跃</div>
                    )
                }
            </div>
          )}
        />
        <Select
          className="c7nagile-StoryMap-storyFilter-select c7nagile-StoryMap-storyFilter-prioritySelect"
          dataSet={selectDataSet}
          name="prioritys"
          placeholder="优先级"
          dropdownMatchSelectWidth={false}
          searchable
          multiple
          maxTagCount={3}
          maxTagTextLength={8}
          maxTagPlaceholder={(restValues) => `+${restValues.length}...`}
        />
        <Select
          className="c7nagile-StoryMap-storyFilter-select c7nagile-StoryMap-storyFilter-componentSelect"
          dataSet={selectDataSet}
          name="components"
          placeholder="模块"
          dropdownMatchSelectWidth={false}
          searchable
          multiple
          maxTagCount={2}
          maxTagTextLength={8}
          maxTagPlaceholder={(restValues) => `+${restValues.length}...`}
        />
      </Form>
      {
          hasFilter && (
          <>
            <Divider />
            <div className="c7nagile-StoryMap-storyFilter-resetDiv">
              <Button
                className="c7nagile-StoryMap-storyFilter-resetBtn"
                icon="refresh"
                onClick={handleResetFilter}
              >
                重置
              </Button>
            </div>
          </>
          )
      }
    </div>
  );
};

export default observer(StoryFilter);
