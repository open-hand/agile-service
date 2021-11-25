import React from 'react';
import { observer } from 'mobx-react';
import useFormatMessage from '@/hooks/useFormatMessage';

function SprintVisibleIssue({
  data: { issueCount },
}) {
  const formatMessage = useFormatMessage('agile.backlog');
  return (
    <div style={{ color: 'var(--text-color3)', marginLeft: '0.26rem' }}>
      {formatMessage({ id: 'visible.issue' }, { issue: issueCount })}
    </div>
  );
}

export default observer(SprintVisibleIssue);
