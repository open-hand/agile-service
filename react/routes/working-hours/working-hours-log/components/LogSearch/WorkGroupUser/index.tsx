import { observer } from 'mobx-react-lite';
import React, {
  useState, useCallback, useImperativeHandle, useMemo, useEffect,
} from 'react';
import {
  DataSet, TextField, Icon, Dropdown, Menu, Tooltip, Spin, Button,
} from 'choerodon-ui/pro';
import classNames from 'classnames';
import styles from './index.less';
import { User } from '@/common/types';
import { StoreProvider, useSelectUserStore } from './stores';
import SelectWorkGroupUser from './SelectWorkGroupUser';

interface OverlayProps {
  setVisible: (visible: boolean) => void
}
interface Props {
  userDropDownRef: React.MutableRefObject<{ selectedUsers: User | undefined } | null>
  projectId?: string
}

const Overlay: React.FC<OverlayProps> = ({
  setVisible,
}) => {
  const { selectUserStore } = useSelectUserStore();
  const caseSelected = selectUserStore.getSelectedFolders();
  console.log(caseSelected);

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
      <SelectWorkGroupUser />
    </div>
  );
};

const ObserverOverlay = observer(Overlay);

const WorkGroupUser = () => {
  const [visible, setVisible] = useState<boolean>(false);
  const [{ dataLoading, isFirstLoad }, setDataMount] = useState({ dataLoading: false, isFirstLoad: true });
  const [selectedUsers, setSelectedUsers] = useState<User[]>([]);

  const handleClear = useCallback((e) => {
    e.stopPropagation();
  }, []);

  const handleVisibleChange = useCallback((flag) => {
    setVisible(flag);
  }, []);

  return (
    <div className={styles.workGroupUser}>
      <Dropdown
        overlay={(
          <ObserverOverlay
            setVisible={setVisible}
          />
        )}
        trigger={['click'] as any}
        visible={visible}
        // @ts-ignore
        onHiddenBeforeChange={(hidden) => {
          if (!hidden && dataLoading) {
            return false;
          }
          // // 初次打开，等待数据加载完成后再打开下拉框
          // if (!hidden && isFirstLoad) {
          //   setDataMount((oldValue) => ({ ...oldValue, dataLoading: true }));
          //   return false;
          // }
          return true;
        }}
        placement={'topCenter' as any}
        onVisibleChange={handleVisibleChange}
      >
        <Spin spinning={dataLoading} size={'small' as any} style={{ width: '50%' }} wrapperClassName={styles.loading}>
          <div style={{ display: 'flex', alignItems: 'center' }} className={styles.dropDown_trigger}>
            <span
              className={styles.trigger_label}
              style={{
                top: '3px',
                left: '6px',
                fontSize: selectedUsers.length ? '12px' : '13px',
                lineHeight: '23px',
              }}
            >
              筛选成员
            </span>
            <span className={styles.selected}>
              {selectedUsers.map((item) => item.realName).join(',')}
            </span>
            {
            selectedUsers?.length > 0 && (
              <Icon
                type="cancel"
                className={classNames(styles.clear_icon, styles.hasSelected_clearIcon)}
                onClick={handleClear}
              />
            )
          }
            <Icon type="expand_more" className={styles.iconPicker} />
          </div>
        </Spin>
      </Dropdown>
    </div>
  );
};

const ObserverWorkGroupUser = observer(WorkGroupUser);
export default ((props: any) => (
  <StoreProvider {...props}>
    <ObserverWorkGroupUser />
  </StoreProvider>
));
