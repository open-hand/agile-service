import React, { memo } from 'react';
import UserHead from '../../../../../components/UserHead/UserHead';

/**
 * 任务经办人呈现
 * @returns React 函数式组件
 * @param assigneeName
 * @param assigneeId
 * @param imageUrl
 */
function Assignee({
  assigneeId, imageUrl, assigneeName,
  assigneeRealName, assigneeLoginName, 
}) {
  return (
    <div>
      {
          assigneeId ? (
            <UserHead
              hiddenText
              size={32}
              style={{ marginLeft: 8 }}
              user={{
                id: assigneeId,
                name: assigneeName,
                loginName: assigneeLoginName,
                realName: assigneeRealName,
                avatar: imageUrl,
              }}
            />
          ) : (
            <div style={{
              width: 32,
              height: 32,
              flexShrink: 0,
              marginLeft: 8,
              marginBottom: 4,
            }}
            />
          )
        }
    </div>
  );
}
export default memo(Assignee);
