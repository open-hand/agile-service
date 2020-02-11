import React from 'react';
import { observer } from 'mobx-react';
import BacklogStore from '@/stores/project/backlog/BacklogStore';

function SprintVisibleIssue({
  data: { sprintId },
}) {
  const issueList = BacklogStore.getIssueListBySprintId(sprintId);
  const issueCount = issueList.length || 0;
  return (
    <div style={{ color: 'rgba(0, 0, 0, 0.65)', marginLeft: '0.26rem' }}>
      {`${issueCount}个问题可见`}
    </div>
  );
}


export default observer(SprintVisibleIssue);
