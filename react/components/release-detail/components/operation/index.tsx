import React from 'react';
import {
  Dropdown, Menu, Button, Modal,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import { Placements } from 'choerodon-ui/pro/lib/dropdown/enum';
import styles from './index.less';
import { useReleaseDetailContext } from '../../stores';

function Operation() {
  const {
    disabled, store, projectId, events,
  } = useReleaseDetailContext();
  const getMenu = () => (
    <Menu onClick={({ key }) => {
      switch (key) {
        default:
          break;
      }
    }}
    >
      <Menu.Item key="feature">导出</Menu.Item>
      <Menu.Item key="del">删除</Menu.Item>
    </Menu>
  );
  return (
    <div
      className={styles.operation}
    >
      {!disabled && (
        <Dropdown overlay={getMenu()} trigger={['click' as Action]} placement={'bottomRight' as Placements}>
          <Button icon="more_vert" />
        </Dropdown>
      )}

    </div>
  );
}
export default observer(Operation);
