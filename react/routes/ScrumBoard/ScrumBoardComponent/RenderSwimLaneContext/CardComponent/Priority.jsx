import React, { memo } from 'react';
import { Tooltip } from 'choerodon-ui';

function Priority({ priorityVO }) {
  return (
    <Tooltip>
      <p
        style={{
          background: `${priorityVO ? priorityVO.colour : '#FFFFFF'}1F`,
          color: priorityVO ? priorityVO.colour : '#FFFFFF',
          textAlign: 'center',
          marginLeft: '8px',
          minWidth: 16,
          maxWidth: 46,
          paddingLeft: 2,
          paddingRight: 2,
          height: 20,
          borderRadius: 2,
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
        }}
      >
        {priorityVO && priorityVO.name}
      </p>
    </Tooltip>
  );
}
export default memo(Priority);
