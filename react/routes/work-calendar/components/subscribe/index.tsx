import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import copy from 'copy-to-clipboard';
import {
  Button, message, Modal, Spin,
} from 'choerodon-ui/pro';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
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

  const handleCreate = useCallback(async () => {
    const res = await orgWorkCalendarApi.createSubscribeUuid();
    if (res) {
      setUuid(res);
    }
  }, []);

  return (
    <div className={Style.wrap}>
      {uuid ? ([
        <div>
          <span>将订阅链接添加到您的Outlook等日历应用中，直接点击复制以下链接即可。</span>
          {/* <span className={Style.more}>了解如何使用？</span> */}
        </div>,
        <div role="none" className={Style.urlWrap} onClick={handleCopyUrl}>
          {url ? <span>{url}</span> : <Spin />}
        </div>,
      ]) : ([
        <div>
          <span>将订阅链接添加到您的Outlook等日历应用中，点击“开启订阅”生成订阅链接。</span>
        </div>,
        <div>
          <Button
            onClick={handleCreate}
            color={ButtonColor.primary}
            className={Style.createBtn}
          >
            开启订阅
          </Button>
        </div>,
      ])}
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
