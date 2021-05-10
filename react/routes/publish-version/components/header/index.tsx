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
import Summary from './components/summary';
import styles from './index.less';

const { Column } = Table;

function PublishVersionHeader() {
  const { prefixCls, store } = usePublishVersionContext();
  const text = store.getCurrentData.statusCode;
  const status = VERSION_STATUS_TYPE[text] || {};
  return (
    <div className={styles.header}>
      <div className={styles.left}>
        {/* {store.getCurrentData.versionAlias || ''} */}
        <Summary />
        <p style={{ marginBottom: 0, marginLeft: '.12rem', minWidth: 60 }}>
          <span
            style={{
              color: '#fff',
              background: status.color,
              display: 'inline-block',
              lineHeight: '20px',
              height: '20px',
              borderRadius: '2px',
              padding: '0 2px',
              fontSize: '13px',
            }}
          >
            <div style={{ transform: 'scale(.8)' }}>{status.name}</div>
          </span>
        </p>
      </div>
      <CustomTabs
        customType="default"
        data={[{ name: '详情', value: 'detail' }, { name: '版本对比', value: 'diff' }, { name: '查看版本信息', value: 'info' }]}
        onChange={(e, name, v) => {
          store.setCurrentMenu(v);
        }}
      />
    </div>
  );
}
export default observer(PublishVersionHeader);
