import React, {
  useMemo, useCallback, useState, useImperativeHandle,
} from 'react';
import { observer } from 'mobx-react-lite';
import { filter, find } from 'lodash';
import { DataSet, TextField } from 'choerodon-ui/pro';
import {
  Dropdown, Icon, Menu,
} from 'choerodon-ui';
import UserTag from '@/components/tag/user-tag';

import { User } from '@/common/types';
import { userApiConfig } from '@/api';
import styles from './index.less';

interface OverlayProps {
  defaultAssignee: User | undefined,
  setVisible: (visible: boolean) => void
  setSelectedUser: (user: User | undefined) => void
  selectedUser: User | undefined
}
interface Props {
  userDropDownRef: React.MutableRefObject<{ selectedUser: User | undefined } | null>
  defaultAssignee: User | undefined,
}

const Overlay: React.FC<OverlayProps> = ({
  setVisible, defaultAssignee, setSelectedUser, selectedUser,
}) => {
  const [filterStr, setFilterStr] = useState<string | null | undefined>();

  const userListDs = useMemo(() => new DataSet({
    autoQuery: true,
    pageSize: 10,
    transport: {
      // @ts-ignore
      read: ({ params }) => userApiConfig.getAllInProject(params?.param, params.page),
    },
  }), []);

  const handleSearchUser = useCallback((value) => {
    setFilterStr(value);
    userListDs.setQueryParameter('param', value);
    userListDs.query();
  }, [userListDs]);

  const handleClick = useCallback((e) => {
    e.stopPropagation();
  }, []);
  const handleUserChange = useCallback(({ key, domEvent }) => {
    if (key === 'loadMore') {
      setVisible(true);
      userListDs.queryMore(userListDs.currentPage + 1);
      return;
    }
    if (key === 'noContentTip') {
      setVisible(true);
      return;
    }
    setVisible(false);
    setSelectedUser(find([...(defaultAssignee ? [defaultAssignee] : []), ...userListDs.toData()] as User[], { id: key }));
  }, [defaultAssignee, setSelectedUser, setVisible, userListDs]);

  return (
    <div className={styles.overlay}>
      <div style={{ background: '#FFF' }}>
        <div role="none" onClick={handleClick} className={styles.searchDiv}>
          <TextField clearButton placeholder="输入文字以进行过滤" value={filterStr} onChange={handleSearchUser} />
        </div>
      </div>
      <Menu
        style={{
          background: '#fff',
          boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0.20), 0 8px 10px 1px rgba(0, 0, 0, 0.14), 0 3px 14px 2px var(--divider)',
          borderRadius: '2px',
        }}
        onClick={handleUserChange}
        className={styles.dropdown_menu}
        selectable={false}
      >
        {
          !filter(userListDs.toData(), (user: User) => user.id !== selectedUser?.id).length && (
            <Menu.Item key="noContentTip" className={styles.dropdown_menu_noContentTipItem}>
              <div className={styles.dropdown_menu_noContentTipItem_tip}>无匹配结果</div>
            </Menu.Item>
          )
        }
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
    </div>
  );
};

const ObserverOverlay = observer(Overlay);

const UserDropDown: React.FC<Props> = ({ userDropDownRef, defaultAssignee }) => {
  const [visible, setVisible] = useState<boolean>(false);
  const [selectedUser, setSelectedUser] = useState<User | undefined>(defaultAssignee);

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
    <div id="userDropdown-container">
      <Dropdown
        overlay={(
          <ObserverOverlay
            setVisible={setVisible}
            defaultAssignee={defaultAssignee}
            setSelectedUser={setSelectedUser}
            selectedUser={selectedUser}
          />
      )}
        trigger={['click']}
        visible={visible}
        onVisibleChange={handleVisibleChange}
      >
        <div style={{ display: 'flex', alignItems: 'center' }}>
          {
          selectedUser ? (
            <>
              <UserTag
                data={selectedUser}
                showText={false}
                tooltip={false}
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

    </div>

  );
};

export default observer(UserDropDown);
