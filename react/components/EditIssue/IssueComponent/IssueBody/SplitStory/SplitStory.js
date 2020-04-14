/* eslint-disable no-restricted-globals */
import React, { useEffect, useState } from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { Progress, Spin } from 'choerodon-ui';
import { getSubStoryByFeature } from '@/api/FeatureApi';
import IssueItem from './IssueItem';

function SplitStory(props) {
  const { store } = props;
  const [loading, setLoading] = useState(true);
  const [data, setData] = useState([]);
  useEffect(() => {
    const loadData = async () => {
      const Data = await getSubStoryByFeature(store.getIssue.issueId);
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
      <div className="c7n-subTask-progress">
        <Progress percent={isNaN(progress) ? 0 : progress} style={{ marginRight: 5 }} />
        已完成
      </div>
      {storyList.map(issue => <IssueItem issue={issue} />)}
    </Spin>
  );
}

export default SplitStory;
