import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { issueApi } from '@/api';
import Star from '@/components/tag/star';
import { useLockFn } from 'ahooks';

const FieldStar = ({
  disabled, store, onUpdate, reloadIssue,
}) => {
  const issue = store.getIssue;
  const { issueId, starBeacon } = issue;
  const handleStarClick = useLockFn(async () => {
    starBeacon ? await issueApi.unstar(issueId) : await issueApi.star(issueId);
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
      activeTooltip="取消"
      inActiveTooltip="关注"
      active={starBeacon}
    />
  );
};

export default observer(FieldStar);
