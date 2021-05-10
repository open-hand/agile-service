import React from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Menu, Table, Tooltip, Modal,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  omit,
} from 'lodash';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import TableDropMenu from '@/common/TableDropMenu';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi, versionApi } from '@/api';
import VERSION_STATUS_TYPE from '@/constants/VERSION_STATUS_TYPE';
import SideNav from '@/components/side-nav';
import { usePublishVersionContext } from '../../stores';

import Detail from './components/detail';
import LinkVersion from './components/link-version';
import styles from './index.less';

const { Column } = Table;
const TooltipButton: React.FC<{ title?: string } & Omit<ButtonProps, 'title'>> = ({
  title, children, disabled, ...otherProps
}) => {
  if (title && disabled) {
    return <Tooltip title={title}><Button disabled={disabled} {...omit(otherProps, 'onClick')}>{children}</Button></Tooltip>;
  }
  return <Button {...otherProps}>{children}</Button>;
};

function PublishVersionBody() {
  const { prefixCls, store } = usePublishVersionContext();
  const menu = store.getCurrentMenu;
  switch (menu) {
    case 'detail':
      return (
        <div className={styles.body}>
          <Detail />
          <LinkVersion />
        </div>
      );
    case 'diff':
      return <div className={styles.body_border}>对比</div>;
    case 'info':
      return <div className={styles.body_border}>版本信息</div>;
    default:
      return <div>00</div>;
  }
}
export default observer(PublishVersionBody);
