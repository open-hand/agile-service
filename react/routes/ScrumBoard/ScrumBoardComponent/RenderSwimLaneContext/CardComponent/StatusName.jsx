import React, { memo } from 'react';
import { Tooltip } from 'choerodon-ui/pro';

/**
 * 任务状态呈现
 * @returns React 函数式组件
 * @param statusName
 * @param categoryCode
 */
function StatusName({ statusName, categoryCode }) {
  const renderStatusBackground = (parameters) => {
    if (parameters === 'todo') {
      return 'rgb(255, 177, 0)';
    } else if (parameters === 'doing') {
      return 'rgb(77, 144, 254)';
    } else if (parameters === 'done') {
      return 'rgb(0, 191, 165)';
    } else {
      return 'gray';
    }
  };
  return (
    <Tooltip title={`状态: ${statusName}`}>
      <div
        style={{
          borderRadius: 2,
          paddingLeft: 4,
          paddingRight: 4,
          background: renderStatusBackground(categoryCode),
          color: 'white',
          maxWidth: 50,
          minWidth: 20,
          textAlign: 'center',
          height: 20,
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
        }}
      >
        {statusName}
      </div>
    </Tooltip>
  );
}
export default memo(StatusName);
