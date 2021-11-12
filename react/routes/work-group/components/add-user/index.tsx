import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import { StoreProvider } from './stores';
import Content from './Content';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';

const addKey = Modal.key();

interface Props {
  workGroupId: string
  // eslint-disable-next-line react/require-default-props
  refresh?(): void,
}

const AddUserIndex = (props: Props) => (
  <StoreProvider {...props}>
    <Content />
  </StoreProvider>
);

function openAddUser(props: Props) {
  Modal.open({
    title: '加入成员',
    key: addKey,
    style: { width: MODAL_WIDTH.small },
    drawer: true,
    children: <AddUserIndex {...props} />,
  });
}

export default openAddUser;
