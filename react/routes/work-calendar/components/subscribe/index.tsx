import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import copy from 'copy-to-clipboard';
import { Modal, Spin, message } from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import Style from './index.less';
import { orgWorkCalendarApi } from '@/api/OrgWorkCalendar';
import { getOrganizationId } from '@/utils/common';

const modalKey = Modal.key();

const SubscribeContent = () => {
  const [uuid, setUuid] = useState('');
  const url = useMemo(() => {
    if (!uuid) {
      return '';
    }
    // @ts-ignore
    // eslint-disable-next-line no-underscore-dangle
    const { API_HOST } = window._env_;
    return (
      `${API_HOST}/agile/v1/organizations/${getOrganizationId()}/work_calendar_subscribe/${uuid}`
    );
  }, [uuid]);

  const loadUrl = useCallback(async () => {
    const res = await orgWorkCalendarApi.loadSubscribeUuid();
    if (res) {
      setUuid(res);
    }
  }, []);

  useEffect(() => {
    loadUrl();
  }, [loadUrl]);

  const handleCopyUrl = () => {
    if (url) {
      copy(url);
      message.info('复制成功！');
    }
  };

  return (
    <div className={Style.wrap}>
      <div>
        <span>将订阅链接添加到您的Outlook等日历应用中，直接点击复制以下链接即可。</span>
        {/* <span className={Style.more}>了解如何使用？</span> */}
      </div>
      <div role="none" className={Style.urlWrap} onClick={handleCopyUrl}>
        {url ? <span>{url}</span> : <Spin />}
      </div>
    </div>
  );
};

const openModal = () => {
  Modal.open({
    key: modalKey,
    style: {
      width: 600,
    },
    movable: false,
    okCancel: false,
    title: '订阅到本地',
    okText: '关闭',
    children: <SubscribeContent />,
  });
};

export default openModal;
