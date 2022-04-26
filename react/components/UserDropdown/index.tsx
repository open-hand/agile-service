import React, {
  useMemo, useCallback, useState, useImperativeHandle, useRef, useEffect, isValidElement, cloneElement, ReactNode, useContext,
} from 'react';
import { observer, useComputed } from 'mobx-react-lite';
import { filter, find } from 'lodash';
import {
  DataSet, TextField, Icon, Menu, Tooltip, Spin, Button,
} from 'choerodon-ui/pro';
import Trigger from 'choerodon-ui/pro/lib/trigger';
import builtinPlacements from 'choerodon-ui/pro/lib/dropdown/placements';
import ConfigContext from 'choerodon-ui/lib/config-provider/ConfigContext';
import {
  useCreation, useDebounceFn, useMount, useWhyDidYouUpdate,
} from 'ahooks';
import UserTag from '@/components/tag/user-tag';

import { User } from '@/common/types';
import { userApiConfig } from '@/api';
import styles from './index.less';

interface OverlayProps {
  userListDs: DataSet
  inputRef: React.MutableRefObject<TextField | undefined>
  // defaultAssignee: User | undefined,
  onSearch: (value: string) => void,
  setVisible: (visible: boolean) => void
  // selectedUser: User | undefined
}
export type IUserDropDownChangeSelectUser = (user?: Partial<User> & { id: string }) => void;
export interface IUserDropDownProps {
  userDropDownRef: React.MutableRefObject<{ selectedUser: User | undefined, changeSelect: IUserDropDownChangeSelectUser } | null>
  defaultAssignee: User | undefined,
  /** 选中的经办人 受控值 */
  assigneeSelected?: Partial<User> & { id: string },
  onChange?: (user?: User) => void
  projectId?: string
}

const Overlay: React.FC<OverlayProps> = ({
  setVisible, userListDs, onSearch, inputRef,
}) => {
  const handleClick = useCallback((e) => {
    e.stopPropagation();
  }, []);
  const handleUserChange = useCallback(({ key, domEvent }) => {
    if (key === 'loadMore') {
      return;
    }
    if (key === 'noContentTip') {
      setVisible(true);
      return;
    }
    setVisible(false);
    const newSelectRecord = userListDs.find((r) => r.get('id') === key);
    newSelectRecord && userListDs.select(newSelectRecord);

    // setSelectedUser(find([...(defaultAssignee ? [defaultAssignee] : []), ...userListDs.toData()] as User[], { id: key }));
  }, [setVisible, userListDs]);
  const { run: handleInput } = useDebounceFn((value) => onSearch(value), { wait: 410 });
  return (
    <div
      role="none"
      id="agile-userDropdown-overlay"
      className={styles.overlay}
      onClick={(e) => {
        e.stopPropagation();
        e.preventDefault();
      }}
    >
      <div style={{ background: '#FFF' }}>
        <div role="none" onClick={handleClick} className={styles.searchDiv}>
          <TextField ref={inputRef as any} clearButton placeholder="输入文字以进行过滤" onChange={handleInput} autoFocus valueChangeAction={'input' as any} />
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
          !userListDs.filter((r) => !r.isSelected).length && (
            <Menu.Item key="noContentTip" className={styles.dropdown_menu_noContentTipItem}>
              <div className={styles.dropdown_menu_noContentTipItem_tip}>无匹配结果</div>
            </Menu.Item>
          )
        }
        {
          userListDs.filter((r) => !r.isSelected).map((record) => {
            const user: User & { tooltip?: string } = record.toData();
            return (
              <Menu.Item key={user.id}>
                <Tooltip title={user.tooltip || `${user.ldap ? `${user.realName}(${user.loginName})` : `${user.realName}(${user.email})`}`}>
                  <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                    <UserTag
                      data={user}
                      tooltip={false}
                    />
                  </div>
                </Tooltip>
              </Menu.Item>
            );
          })
        }
        {
          userListDs.totalPage > userListDs.currentPage && (
            <Menu.Item
              key="loadMore"
              className={styles.dropdown_loadMoreMenuItem}
            >
              <Button
                onClick={(e) => {
                  e.stopPropagation();
                  userListDs.queryMore(userListDs.currentPage + 1);
                }}
                style={{ margin: '-4px -12px', width: 'calc(100% + 24px)' }}
              >
                加载更多
              </Button>
              {/* <div style={{ textAlign: 'center' }}>查看更多</div> */}
            </Menu.Item>
          )
        }
      </Menu>
    </div>
  );
};

const ObserverOverlay = observer(Overlay);
function changeDatasetSelect(dataset: DataSet, user?: IUserDropDownProps['assigneeSelected']) {
  if (user) {
    const record = dataset.find((r) => r.get('id') === user.id) || dataset.create(user);
    record.status === 'add' && dataset.setState('init', true);
    dataset.select(record);
    return;
  }
  const record = dataset.selected.length && dataset.selected[0];
  record && dataset.unSelect(record);
}
const UserDropDown: React.FC<IUserDropDownProps> = ({
  userDropDownRef, projectId, onChange, ...props
}) => {
  const { getProPrefixCls } = useContext(ConfigContext);
  const prefixCls = getProPrefixCls('dropdown');
  const inputRef = useRef<TextField>();
  const timeoutIdRef = useRef<number>();
  const [visible, setVisible] = useState<boolean>(false);
  const triggerRef = useRef<any>();
  const defaultAssignee = useCreation(() => props.defaultAssignee, []);
  const [{ dataLoading, isFirstLoad }, setDataMount] = useState({ dataLoading: false, isFirstLoad: true });
  const events = useCreation(() => ({}) as Pick<IUserDropDownProps, 'onChange'>, []);
  events.onChange = onChange;
  const userListDs = useMemo(() => {
    let init = true;
    const ds = new DataSet({
      autoQuery: false,
      pageSize: 20,
      selection: 'single' as any,
      data: [defaultAssignee].filter(Boolean) as User[],
      transport: {
        read: ({ params, data: requestData }: any) => ({
          ...userApiConfig.project(projectId).getProjectUsersWithoutDisable({ ...params, ...requestData }),
          transformResponse: (res) => {
            const data = JSON.parse(res);
            const content = data.content?.filter((item: any) => item.enabled);
            return { ...data, content };
          },
        }),
      },
      events: {
        load: ({ dataSet }: any) => {
          if (init && defaultAssignee) {
            changeDatasetSelect(dataSet, defaultAssignee);
          }
          init = false;
        },
        select: ({ dataSet, record }: { dataSet: DataSet, record: any }) => {
          if (!dataSet.getState('init')) {
            dataSet.created.length && dataSet.delete(dataSet.created[0], false);
            events.onChange && events.onChange(record.toData());
          }
          dataSet.setState('init', false);
        },
        unSelect: ({ dataSet, record }: { dataSet: DataSet, record: any }) => {
          dataSet.created.length && dataSet.delete(dataSet.created[0], false);
          events.onChange && events.onChange();
        },
      },
    });
    return ds;
  }, [defaultAssignee, events, projectId]);

  const selectedUser = useComputed(() => userListDs.selected.length && userListDs.selected[0].toData(), []);
  const handleChangeSelect = useCallback((user?: IUserDropDownProps['assigneeSelected']) => {
    changeDatasetSelect(userListDs, user);
  }, [userListDs]);
  useImperativeHandle(userDropDownRef, () => ({
    selectedUser,
    changeSelect: handleChangeSelect,
  }));

  const handleClear = useCallback((e) => {
    e.stopPropagation();
    const record = userListDs.selected.length && userListDs.selected[0];
    record && userListDs.unSelect(record);
  }, [userListDs]);

  const handleHiddenChange = useCallback((newHidden) => {
    setVisible(!newHidden);
    if (!newHidden) {
      timeoutIdRef.current = setTimeout(() => {
        inputRef.current?.focus();
      }, 120) as unknown as number;
    } else {
      clearTimeout(timeoutIdRef.current);
    }
  }, []);
  const handleSearchUser = useCallback((value) => {
    userListDs.setQueryParameter('param', value);
    setDataMount((oldValue: any) => ({ ...oldValue, dataLoading: true }));
    const currentSelected = userListDs.selected.length && userListDs.selected[0].toData();
    return userListDs.query().then((res) => {
      setDataMount((oldValue: any) => ({ ...oldValue, isFirstLoad: false, dataLoading: false }));
      triggerRef.current?.forcePopupAlign();
      userListDs.setState('init', true);
      handleChangeSelect(currentSelected);
    });
  }, [handleChangeSelect, userListDs]);

  useEffect(() => {
    if (dataLoading && isFirstLoad) {
      handleSearchUser(undefined).then(() => {
        setVisible(true);
      });
    }
  }, [dataLoading, handleSearchUser, isFirstLoad]);

  const handleClick = useCallback((e) => {
    setVisible(true);
  }, []);
  const getContent = useCallback((...popupProps): ReactNode => (
    <ObserverOverlay
      inputRef={inputRef}
      userListDs={userListDs}
      setVisible={setVisible}
      onSearch={handleSearchUser}
    />
  ), [handleSearchUser, userListDs]);
  const renderPopupContent = useCallback((...popupProps) => {
    const content = getContent(...popupProps);
    if (isValidElement<any>(content)) {
      return cloneElement<any>(content, {
        onClick: handleClick,
      });
    }
    return null;
  }, [getContent, handleClick]);
  const hidden = !visible;
  return (
    <div id="userDropdown-container">
      <Trigger
        prefixCls={prefixCls}
        ref={triggerRef}
        popupContent={renderPopupContent}
        builtinPlacements={builtinPlacements}
        action={['click'] as any}
        popupHidden={hidden}
        onPopupHiddenBeforeChange={(newHidden) => {
          if (!newHidden && dataLoading) {
            return false;
          }
          // 初次打开，等待数据加载完成后再打开下拉框
          if (!newHidden && isFirstLoad) {
            setDataMount((oldValue) => ({ ...oldValue, dataLoading: true }));
            return false;
          }
          return true;
        }}
        popupPlacement="topCenter"
        onPopupHiddenChange={handleHiddenChange}
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
      </Trigger>

    </div>

  );
};

export default observer(UserDropDown);
