import React from 'react';
import { CheckBox } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import styles from './index.less';
import { useSelectUserStore } from '../stores';
import { User } from '@/common/types';

interface Props {
}
const UserList: React.FC<Props> = () => {
  const { userListDs } = useSelectUserStore();
  return (
    <div className={styles.list}>
      {
        userListDs.toData().map((user: User) => (
          <div className={styles.user_item}>
            <CheckBox dataSet={userListDs} name="user" value={user.id}>
              <div>
                <span>{user.realName}</span>
                <span>{user.loginName}</span>
              </div>
            </CheckBox>
          </div>
        ))
      }
    </div>

  );
};

export default observer(UserList);
