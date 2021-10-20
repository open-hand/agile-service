import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { useLockFn } from 'ahooks';
import { issueApi } from '@/api';
import Star from '@/components/tag/star';

const FieldStar = ({
  disabled, store, onUpdate, reloadIssue,
}) => {
  const issue = store.getIssue;
  const { issueId, starBeacon } = issue;
  const handleStarClick = useLockFn(async () => {
    starBeacon ? await issueApi.project(store.projectId).unstar(issueId) : await issueApi.project(store.projectId).star(issueId);
    if (onUpdate) {
      onUpdate();
    }
    if (reloadIssue) {
      reloadIssue(issueId);
    }
  }, [issueId, onUpdate, reloadIssue, starBeacon]);
  return (
    <Star
      disabled={disabled}
      onClick={handleStarClick}
      style={{ margin: '6px 5px 0' }}
      activeTooltip="取消关注"
      inActiveTooltip="关注"
      active={starBeacon}
    />
  );
};

export default observer(FieldStar);
