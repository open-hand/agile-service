import React from 'react';
import { observer } from 'mobx-react';

function SprintVisibleIssue({
  data: { issueCount },
}) {
  return (
    <div style={{ color: 'var(--text-color3)', marginLeft: '0.26rem' }}>
      {`${issueCount}个工作项可见`}
    </div>
  );
}

export default observer(SprintVisibleIssue);
