import React, { memo } from 'react';
import { Tooltip } from 'choerodon-ui/pro';

/**
 * 任务经办人呈现
 * @returns React 函数式组件
 * @param summary
 */
function Summary({ summary }) {
  return (
    <Tooltip title={summary} placement="topLeft">
      <div className="textDisplayTwoColumn">
        {summary}
      </div>
    </Tooltip>
  );
}
export default memo(Summary);
