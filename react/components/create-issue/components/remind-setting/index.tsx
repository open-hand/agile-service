import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { StoreProvider } from './stores';
import Content from './Content';

interface Props {
  issueId: string,
}

const key = Modal.open();

const RemindSettingIndex = (props: Props) => (
  <StoreProvider {...props}>
    <Content />
  </StoreProvider>
);

function openRemindSetting(props: Props) {
  Modal.open({
    key,
    title: '工作项提醒',
    style: {
      width: MODAL_WIDTH.middle,
    },
    children: <RemindSettingIndex {...props} />,
  });
}

export default openRemindSetting;
