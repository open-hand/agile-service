import React, { memo } from 'react';
import { Tooltip } from 'choerodon-ui';

/**
 * 任务经办人呈现
 * @returns React 函数式组件
 * @param summary
 */
function Summary({ summary }) {
  return (
    <Tooltip title={summary} placement="topLeft">
      <p className="textDisplayTwoColumn">
        {summary}
      </p>
    </Tooltip>
  );
}
export default memo(Summary);
