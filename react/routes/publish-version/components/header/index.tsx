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
import { CustomTabs } from '@choerodon/components';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import TableDropMenu from '@/common/TableDropMenu';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi, versionApi } from '@/api';
import VERSION_STATUS_TYPE from '@/constants/VERSION_STATUS_TYPE';
import SideNav from '@/components/side-nav';
import { usePublishVersionContext } from '../../stores';
import styles from './header.less';

const { Column } = Table;

function PublishVersionHeader() {
  const { prefixCls, tableDataSet } = usePublishVersionContext();
  const text = '' as any;
  const status = VERSION_STATUS_TYPE[text as keyof typeof VERSION_STATUS_TYPE] || {};

  return (
    <div className={styles.header}>
      <div className={styles.left}>
        版本一

        <p style={{ marginBottom: 0, minWidth: 60 }}>
          <span
            style={{
              color: '#fff',
              background: status.color,
              display: 'inline-block',
              lineHeight: '16px',
              height: '16px',
              borderRadius: '2px',
              padding: '0 2px',
              fontSize: '13px',
            }}
          >
            <div style={{ transform: 'scale(.8)' }}>{status.name}</div>
          </span>
        </p>
      </div>
      <CustomTabs customType="default" data={[{ name: '详情', value: 'detail' }, { name: '版本对比', value: 'diff' }, { name: '查看版本信息', value: 'info' }]} />
    </div>
  );
}
export default observer(PublishVersionHeader);
