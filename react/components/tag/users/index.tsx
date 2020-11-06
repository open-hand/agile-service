import React from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import UserHead from '@/components/UserHead';
import { User } from '@/common/types';
import styles from './index.less';

interface UsersProps {
  data: User[]
  maxTagCount?: number
}
const Users: React.FC<UsersProps> = ({ data, maxTagCount = 3 }) => {
  if (!data) {
    return null;
  }
  const visibleData = data.slice(0, maxTagCount);
  const hiddenData = data.slice(maxTagCount);
  return (
    <div style={{
      display: 'inline-flex',
      alignItems: 'center',
    }}
    >
      {visibleData.map((user: User) => (
        <UserHead
          // size={20}
          key={user.id}
          // @ts-ignore
          style={{ display: 'inline-block' }}
          hiddenText
          user={user}
        />
      ))}
      {
        hiddenData.length > 0 && (
          <Tooltip
          // @ts-ignore
            popupCls={styles.tooltip}
            title={hiddenData.map((user) => (
              <UserHead
              // @ts-ignore
                user={user}
                style={{
                  marginBottom: '.05rem',
                  cursor: 'pointer',
                }}
              />
            ))}
            theme="light"
          >
            <div className={styles.more}>
              {`+${hiddenData.length}`}
            </div>
          </Tooltip>
        )
      }
    </div>
  );
};
export default Users;
