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
              size={24}
              style={{ marginLeft: 0 }}
              avatarStyle={{ fontSize: '13px' }}
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
              width: 24,
              height: 24,
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
