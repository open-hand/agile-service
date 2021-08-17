/* eslint-disable no-restricted-globals */
import React from 'react';
import { Progress, Tooltip } from 'choerodon-ui';
import IssueItem from './IssueItem';

function SplitStory(props) {
  const {
    outside, organizationId, splitStoryData,
  } = props;

  const { totalStoryPoints, completedStoryPoints, storyList = [] } = splitStoryData || {};
  const progress = parseInt(completedStoryPoints * 100 / totalStoryPoints, 10);
  return (
    <div>
      <span style={{ fontSize: '16px', fontWeight: 500 }}>故事点完成进度</span>
      <Tooltip title={(
        <div>
          <p>{`已完成故事点：${completedStoryPoints || 0}`}</p>
          <p>{`总故事点：${totalStoryPoints || 0}`}</p>
        </div>
      )}
      >
        <div className="c7n-subTask-progress">
          <Progress percent={isNaN(progress) ? 0 : progress} style={{ marginRight: 5 }} />
          {
            !!(completedStoryPoints && totalStoryPoints) && (
              <>
                已完成
              </>
            )
          }
        </div>
      </Tooltip>
      {storyList.map((issue) => <IssueItem issue={issue} outside={outside} organizationId={organizationId} />)}
    </div>
  );
}

export default SplitStory;
