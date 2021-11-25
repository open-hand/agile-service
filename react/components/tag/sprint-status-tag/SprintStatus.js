import React from 'react';
import { observer } from 'mobx-react';
import './SprintStatus.less';
import useFormatMessage from '@/hooks/useFormatMessage';

const prefix = 'c7n-sprintStatus';

function SprintStatus({
  data: { statusCode, planning },
}) {
  const formatMessage = useFormatMessage();
  let sprintStatusName = formatMessage({ id: 'agile.backlog.no.start' });
  if (statusCode === 'started') {
    sprintStatusName = formatMessage({ id: 'agile.common.active' });
  } else if (statusCode === 'closed') {
    sprintStatusName = formatMessage({ id: 'agile.common.complete' });
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
