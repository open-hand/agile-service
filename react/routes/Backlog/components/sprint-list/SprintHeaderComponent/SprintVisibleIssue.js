import React from 'react';
import { observer } from 'mobx-react';

function SprintVisibleIssue({
  data: { issueCount },
}) {
  return (
    <div style={{ color: 'rgba(0, 0, 0, 0.65)', marginLeft: '0.26rem' }}>
      {`${issueCount || 0}个问题可见`}
    </div>
  );
}


export default observer(SprintVisibleIssue);
