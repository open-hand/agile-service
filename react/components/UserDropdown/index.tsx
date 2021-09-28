import React, {
  useMemo, useCallback, useState, useImperativeHandle, useRef, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import { filter, find } from 'lodash';
import {
  DataSet, TextField, Icon, Dropdown, Menu, Tooltip, Spin,
} from 'choerodon-ui/pro';
import UserTag from '@/components/tag/user-tag';

import { User } from '@/common/types';
import { userApiConfig } from '@/api';
import styles from './index.less';

interface OverlayProps {
  userListDs: DataSet
  defaultAssignee: User | undefined,
  setDataMount: (newValue: any) => any
  setVisible: (visible: boolean) => void
  setSelectedUser: (user: User | undefined) => void
  selectedUser: User | undefined
}
interface Props {
  userDropDownRef: React.MutableRefObject<{ selectedUser: User | undefined } | null>
  defaultAssignee: User | undefined,
}

const Overlay: React.FC<OverlayProps> = ({
  setVisible, defaultAssignee, setSelectedUser, selectedUser, userListDs, setDataMount,
}) => {
  const [filterStr, setFilterStr] = useState<string | null | undefined>();

  const handleSearchUser = useCallback((value) => {
    setFilterStr(value);
    userListDs.setQueryParameter('param', value);
    setDataMount((oldValue: any) => ({ ...oldValue, dataLoading: true }));
    setVisible(false);
    const startTime = new Date().getTime();
    userListDs.query().then((res) => {
      setDataMount((oldValue: any) => ({ ...oldValue, dataLoading: false }));
      const diff = new Date().getTime() - startTime;
      setTimeout(() => setVisible(true), Math.max(0, diff - 250));
    });
  }, [setDataMount, setVisible, userListDs]);

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
              <Tooltip title={`${user.ldap ? `${user.realName}(${user.loginName})` : `${user.realName}(${user.email})`}`}>
                <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                  <UserTag
                    data={user}
                    tooltip={false}
                  />
                </div>
              </Tooltip>
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
  const [{ dataLoading, isFirstLoad }, setDataMount] = useState({ dataLoading: false, isFirstLoad: true });
  const [selectedUser, setSelectedUser] = useState<User | undefined>(defaultAssignee);
  const userListDs = useMemo(() => new DataSet({
    autoQuery: false,
    pageSize: 20,
    transport: {
      // @ts-ignore
      read: ({ params }) => userApiConfig.getAllInProject(params?.param, params.page, undefined, params.size),
    },
  }), []);
  useImperativeHandle(userDropDownRef, () => ({
    selectedUser,
  }));
  useEffect(() => {
    if (dataLoading && isFirstLoad) {
      userListDs.query().then(() => {
        setDataMount((oldValue) => ({ ...oldValue, isFirstLoad: false, dataLoading: false }));
        setVisible(true);
      });
    }
  }, [dataLoading, isFirstLoad, userListDs]);
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
            userListDs={userListDs}
            setDataMount={setDataMount}
            setVisible={setVisible}
            defaultAssignee={defaultAssignee}
            setSelectedUser={setSelectedUser}
            selectedUser={selectedUser}
          />
        )}
        trigger={['click'] as any}
        visible={visible}
        // @ts-ignore
        onHiddenBeforeChange={(hidden) => {
          if (!hidden && dataLoading) {
            return false;
          }
          // 初次打开，等待数据加载完成后再打开下拉框
          if (!hidden && isFirstLoad) {
            setDataMount((oldValue) => ({ ...oldValue, dataLoading: true }));
            return false;
          }
          return true;
        }}
        placement={'topCenter' as any}
        onVisibleChange={handleVisibleChange}
      >
        <Spin spinning={dataLoading} size={'small' as any} style={{ width: '50%' }} wrapperClassName={styles.loading}>
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
        </Spin>
      </Dropdown>

    </div>

  );
};

export default observer(UserDropDown);
