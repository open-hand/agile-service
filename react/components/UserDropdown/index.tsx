import React, {
  useMemo, useCallback, useState, useImperativeHandle,
} from 'react';
import { observer } from 'mobx-react-lite';
import { filter, find } from 'lodash';
import { DataSet } from 'choerodon-ui/pro';
import { Dropdown, Icon, Menu } from 'choerodon-ui';
import UserTag from '@/components/tag/user-tag';

import { User } from '@/common/types';
import { userApiConfig } from '@/api';
import styles from './index.less';

interface Props {
  userDropDownRef: React.MutableRefObject<{ selectedUser: User | undefined }>
  defaultAssignee: User | undefined,
}

const UserDropDown: React.FC<Props> = ({ userDropDownRef, defaultAssignee }) => {
  const [visible, setVisible] = useState<boolean>(false);
  const [selectedUser, setSelectedUser] = useState<User | undefined>(defaultAssignee?.id === 'clear' ? undefined : defaultAssignee);

  const userListDs = useMemo(() => new DataSet({
    autoQuery: true,
    pageSize: 10,
    transport: {
      // @ts-ignore
      read: ({ params }) => userApiConfig.getAllInProject(undefined, params.page),
    },
  }), []);

  const handleUserChange = useCallback(({ key, domEvent }) => {
    if (key !== 'loadMore') {
      setVisible(false);
      setSelectedUser(find([...(defaultAssignee ? [defaultAssignee] : []), ...userListDs.toData()] as User[], { id: key }));
    } else {
      setVisible(true);
      userListDs.queryMore(userListDs.currentPage);
    }
  }, [defaultAssignee, userListDs]);

  const userList = (
    <Menu
      style={{
        background: '#fff',
        boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0.20), 0 8px 10px 1px rgba(0, 0, 0, 0.14), 0 3px 14px 2px var(--divider)',
        borderRadius: '2px',
      }}
      onClick={handleUserChange}
      className={styles.dropdown_menu}
    >
      {
        filter(userListDs.toData(), (user: User) => user.id !== selectedUser?.id).map((user: User) => (
          <Menu.Item key={user.id}>
            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
              <UserTag
                data={user}
              />
            </div>
          </Menu.Item>
        ))
      }
      {
        userListDs.totalPage > userListDs.currentPage && (
        <Menu.Item
          key="loadMore"
          className={styles.dropdown_loadMoreMenuItem}
        >
          <div style={{ textAlign: 'center' }}>查看更多</div>
        </Menu.Item>
        )
      }
    </Menu>
  );

  useImperativeHandle(userDropDownRef, () => ({
    selectedUser,
  }));

  const handleClear = useCallback((e) => {
    e.stopPropagation();
    setSelectedUser(undefined);
  }, []);

  const handleVisibleChange = useCallback((flag) => {
    setVisible(flag);
  }, []);

  return (
    <Dropdown
      overlay={userList}
      trigger={['click']}
      visible={visible}
      onVisibleChange={handleVisibleChange}
      // @ts-ignore
      getPopupContainer={((triggerNode) => triggerNode.parentNode)}
    >
      <div style={{ display: 'flex', alignItems: 'center' }}>
        {
          selectedUser ? (
            <>
              <UserTag
                data={selectedUser}
                showText={false}
              />
              <Icon type="close" style={{ fontSize: 14, cursor: 'pointer' }} onClick={handleClear} />
            </>
          ) : <div className={styles.tip}>经办人</div>
        }
        <Icon
          type="arrow_drop_down"
          style={{ fontSize: 16, cursor: 'pointer' }}
        />
      </div>
    </Dropdown>
  );
};

export default observer(UserDropDown);
