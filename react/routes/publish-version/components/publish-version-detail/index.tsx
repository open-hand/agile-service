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
  projectId?: string | number, /** 关联特性时 根据项目id查询特性 projectId = 0  则会有特性筛选项--- 项目select  */
  programId?: string, /** 项目群id */
  disabled?: boolean,
  modal?:IModalProps,
  disableInitStore?: boolean, /** @default false */
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

export function openPublishVersionDetail(id: string) {
  // store.select(id);
  Modal.open({
    drawer: true,
    className: 'c7n-agile-publish-version-detail-modal',
    // className: importStyles.modal,
    // maskClosable: false,
    key: Modal.key(),
    // title: '导入字段',
    style: {
      width: MODAL_WIDTH.middle,
    },
    okText: '导入',
    cancelText: '关闭',
    footer: () => null,
    // footer: (okBtn) => okBtn,
    children: <ReleaseDetailIndex id={id} />,
  });
}
export { ReleaseDetailProps, useReleaseDetailStore };
export default ReleaseDetailIndex;
