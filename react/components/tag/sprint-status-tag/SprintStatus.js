import React from 'react';
import { observer } from 'mobx-react';
import './SprintStatus.less';

const prefix = 'c7n-sprintStatus';

function SprintStatus({
  data: { statusCode, planning },
}) {
  let sprintStatusName = '未开始';
  if (statusCode === 'started') {
    sprintStatusName = '活跃';
  } else if (statusCode === 'closed') {
    sprintStatusName = '已完成';
  }
  if (planning) {
    sprintStatusName = '规划中';
  }
  return (
    <div className={`${prefix} ${prefix}-${statusCode}`}>
      {sprintStatusName}
    </div>
  );
}

export default observer(SprintStatus);
