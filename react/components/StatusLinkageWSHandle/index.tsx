import React, { useCallback } from 'react';
import { WSHandler, Choerodon } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';

const StatusLinkageWSHandle = () => {
  const handleMessage = useCallback((message: string) => {
    const data = JSON.parse(message);
    const { statusCode } = data;
    if (statusCode !== 'success') {
      Choerodon.prompt('状态联动失败');
    }
  }, []);

  return (
    <WSHandler
      messageKey={`agile-execution-link-issue-linkage-${getProjectId()}`}
      onMessage={handleMessage}
    >
      <div />
    </WSHandler>
  );
};

export default StatusLinkageWSHandle;
