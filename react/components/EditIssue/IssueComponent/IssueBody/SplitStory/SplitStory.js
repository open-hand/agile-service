/* eslint-disable no-restricted-globals */
import React, { useEffect, useState } from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { Progress, Spin, Tooltip } from 'choerodon-ui';
import { featureApi } from '@/api';
import { getProjectId } from '@/utils/common';
import IssueItem from './IssueItem';

function SplitStory(props) {
  const { store, outside } = props;
  const { projectId, issueId } = store.getIssue;
  const [loading, setLoading] = useState(true);
  const [data, setData] = useState([]);
  useEffect(() => {
    const loadData = async () => {
      let Data;
      if (outside) {
        Data = await await featureApi.getSplitStoryOutside(issueId, projectId);
      } else {
        Data = getProjectId().toString() !== projectId.toString() ? await featureApi.getSubProjectSplitStory(issueId, projectId) : await featureApi.getSplitStory(issueId);
      }

      batchedUpdates(() => {
        setLoading(false);
        setData(Data);
      });
    };
    loadData();
  }, []);
  const { totalStoryPoints, completedStoryPoints, storyList = [] } = data;
  const progress = parseInt(completedStoryPoints * 100 / totalStoryPoints, 10);
  return (
    <Spin spinning={loading}>
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
      {storyList.map((issue) => <IssueItem issue={issue} />)}
    </Spin>
  );
}

export default SplitStory;
