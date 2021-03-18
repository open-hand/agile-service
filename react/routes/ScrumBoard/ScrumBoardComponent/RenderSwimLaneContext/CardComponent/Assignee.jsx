import React, { memo } from 'react';
import UserTag from '@/components/tag/user-tag';

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
            <UserTag
              showText={false}
              size={28}
              style={{ marginLeft: 0 }}
              data={{
                id: assigneeId,
                tooltip: assigneeName,
                loginName: assigneeLoginName,
                realName: assigneeRealName,
                imageUrl,
              }}
            />
          ) : (
            <div style={{
              width: 28,
              height: 28,
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
