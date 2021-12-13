import React, { memo } from 'react';
import { Tooltip } from 'choerodon-ui/pro';

function Priority({ priorityVO }) {
  return (
    <Tooltip>
      <div
        style={{
          background: `${priorityVO ? priorityVO.colour : '#FFFFFF'}1F`,
          color: priorityVO ? priorityVO.colour : '#FFFFFF',
          textAlign: 'center',
          marginLeft: '10px',
          minWidth: 16,
          maxWidth: 46,
          paddingLeft: 4,
          paddingRight: 4,
          height: 20,
          borderRadius: 2,
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
        }}
      >
        {priorityVO && priorityVO.name}
      </div>
    </Tooltip>
  );
}
export default memo(Priority);
