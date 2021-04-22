import React from 'react';
import { Modal } from 'choerodon-ui/pro/lib';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IModalProps } from '@/common/types';
import IReleaseDetailData from './types';
import store, { ReleaseDetailStore } from './stores/store';
import ReleaseDetail from './ReleaseDetail';
import StoreProvider from './stores';

interface ReleaseDetailProps {
  id: string
  disabled?: boolean,
  modal?: IModalProps,
  onUpdate?: () => void,
  events?: {
    loadAfter?: (data: { piAimData: IReleaseDetailData, featureList?: Array<any> }) => void,
    deleteAfter?: (data: IReleaseDetailData) => void,
  },
}
const useReleaseDetailStore = () => store;
const ReleaseDetailIndex: React.FC<ReleaseDetailProps> = (props) => (
  <StoreProvider {...props}>
    <ReleaseDetail />
  </StoreProvider>
);

export function openPublishVersionDetail(id: string, onOk: () => void) {
  Modal.open({
    drawer: true,
    className: 'c7n-agile-publish-version-detail-modal',
    // className: importStyles.modal,
    // maskClosable: false,
    title: null,
    border: false,
    key: 'publish-version-detail-modal',
    // title: '导入字段',
    style: {
      height: 'calc(100vh - 48px)',
      marginTop: 48,
      width: 850,
    },
    mask: false,
    maskStyle: {
      height: 'calc(100vh - 48px)',
      marginTop: 48,
    },
    footer: () => null,
    // footer: (okBtn) => okBtn,
    children: <ReleaseDetailIndex id={id} onUpdate={onOk} />,
  });
}
export { ReleaseDetailProps, useReleaseDetailStore };
export default ReleaseDetailIndex;
