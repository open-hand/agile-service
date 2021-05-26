import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { CustomTabs } from '@choerodon/components';
import PUBLISH_VERSION_STATUS_TYPE from '@/constants/PUBLISH_VERSION_STATUS_TYPE';
import classNames from 'classnames';
import { usePublishVersionContext } from '../../stores';
import styles from './index.less';

function PublishVersionHeader() {
  const {
    preview, store, customMenu, renderDetailLeftHeader,
  } = usePublishVersionContext();
  const text = store.getCurrentData.statusCode;
  const currentStatus = PUBLISH_VERSION_STATUS_TYPE[text] || {};
  const tabData = useMemo(() => {
    const defaultTabs = [{ name: '详情', value: 'detail' }, { name: '版本对比', value: 'diff' }, { name: '查看版本信息', value: 'info' }];
    if (customMenu) {
      defaultTabs.push(...[...customMenu.entries()].map(([key, { title }]) => ({ name: title, value: key })));
    }
    return defaultTabs;
  }, [customMenu]);
  const renderStatus = (status = currentStatus) => (
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
  );
  return (
    <div className={classNames(styles.header, { [styles.preview]: preview })}>
      <div className={styles.left}>
        {renderDetailLeftHeader ? renderDetailLeftHeader(renderStatus) : [store.getCurrentData.versionAlias || '', renderStatus()]}
      </div>
      {!preview && (
        <CustomTabs
          customType="default"
          key={`CustomTabs-${store.getCurrentMenu}`}
          selectedTabValue={store.getCurrentMenu}
          data={tabData}
          onChange={(e, name, v) => {
            store.setCurrentMenu(v);
          }}
        />
      )}
    </div>
  );
}
export default observer(PublishVersionHeader);
