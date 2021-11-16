import React, { useCallback, useState } from 'react';
import { CheckBox, Button, TextField } from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import { Tooltip } from 'antd';
import classNames from 'classnames';
import styles from './index.less';
import { useSelectUserStore } from '../stores';
import { User } from '@/common/types';

interface WorkGroupVOS {
  children: any[] | null
  id: string
  name: string
  objectVersionNumber: string
  organizationId: string
  parentId: string | 0
  userCount: number
  userId: string
  userIds: string[]
}
interface WorkGroupUser {
  userId: string
  userVO: User
  workGroupIds: string
  workGroupVOS: WorkGroupVOS[]
}
interface Props {
}
const UserList: React.FC<Props> = () => {
  const { userListDs } = useSelectUserStore();
  console.log('toData：');
  console.log(userListDs.toData());
  const [filterStr, setFilterStr] = useState<string | null | undefined>();

  const handleSearchUser = useCallback((value) => {
    setFilterStr(value);
    userListDs.setQueryParameter('param', value);
    // setDataMount((oldValue: any) => ({ ...oldValue, dataLoading: true }));
    // setVisible(false);
    const startTime = new Date().getTime();
    userListDs.query().then((res) => {
      // setDataMount((oldValue: any) => ({ ...oldValue, dataLoading: false }));
      const diff = new Date().getTime() - startTime;
      // setTimeout(() => setVisible(true), Math.max(0, diff - 250));
    });
  }, [userListDs]);

  const handleLoadMore = useCallback(() => {
    userListDs.queryMore(userListDs.currentPage + 1);
  }, [userListDs]);

  return (
    <div className={styles.userList}>
      {
        userListDs.toData().length === 0 && !filterStr ? (
          <div className={styles.empty}>当前工作组暂无数据</div>
        ) : (
          <>
            <div className={styles.search}>
              <TextField clearButton placeholder="筛选成员" value={filterStr} onChange={handleSearchUser} />
            </div>
            <div className={styles.list}>
              {
                userListDs.toData().length > 0 && (
                  <div className={styles.user_item}>
                    <CheckBox dataSet={userListDs} name="user" value="all">
                      <div className={classNames(styles.user_item_text, styles.select_all)}>
                        全选
                      </div>
                    </CheckBox>
                  </div>
                )
              }
              {
                userListDs.toData().map((user: WorkGroupUser) => {
                  const {
                    ldap, realName, loginName, email,
                  } = user.userVO || {};
                  const textShow = ldap ? `${realName}${loginName ? `(${loginName})` : ''}` : `${realName}${email ? `(${email})` : ''}`;
                  return (
                  // <Tooltip title={textShow}>
                    <div className={styles.user_item}>
                      <CheckBox dataSet={userListDs} name="user" value={user.userId}>
                        <div className={styles.user_item_text}>
                          {textShow}
                        </div>
                      </CheckBox>
                    </div>
                  // </Tooltip>
                  );
                })
              }
              {
                filterStr && !userListDs.toData().length && (
                  <div className={styles.filterNoData}>当前搜索下暂无数据</div>
                )
              }
            </div>
            {
              userListDs.totalPage > userListDs.currentPage && (
                <div
                  role="none"
                  onClick={handleLoadMore}
                  className={styles.loadMore}
                >
                  加载更多
                </div>
              )
            }
          </>
        )
      }
    </div>
  );
};

export default observer(UserList);
