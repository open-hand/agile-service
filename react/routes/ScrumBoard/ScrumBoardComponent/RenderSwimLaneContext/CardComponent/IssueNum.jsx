import React, { memo } from 'react';

/**
 * 任务类型呈现
 * @returns React 函数式组件
 * @param issueNum
 * @param completed
 */
function IssueNum({ issueNum, completed }) {  
  return (
    <p
      style={{ marginLeft: 5, marginTop: 2, textDecoration: completed ? 'line-through' : '' }}
      className="textDisplayOneColumn"
    >
      {issueNum}
    </p>
  );
}
export default memo(IssueNum);
