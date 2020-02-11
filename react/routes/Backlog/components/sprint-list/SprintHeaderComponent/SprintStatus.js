import React from 'react';
import { observer } from 'mobx-react';
import './SprintStatus.less';

const prefix = 'c7n-backlog-SprintStatus';

function SprintStatus({
  data: { statusCode },
}) {
  return statusCode === 'started' ? (         
    <div className={`${prefix} ${prefix}-active`}>
      活跃
    </div>
  ) : (         
    <div className={prefix}>
      未开始
    </div>
  );
}

export default observer(SprintStatus);
