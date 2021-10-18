import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import ExportLog from './ExportLog';
import styles from './ExportLog.less';

function openExportLogModal(props: any) {
  Modal.open({
    maskClosable: false,
    key: Modal.key(),
    title: '导出工时日志',
    style: {
      width: MODAL_WIDTH.small,
    },
    className: styles.modal,
    drawer: true,
    children: <ExportLog {...props} />,
    cancelText: '关闭',
    footer: (okBtn: any, cancelBtn: any) => cancelBtn,
    cancelProps: {
      color: 'primary',
    },
  });
}
export { openExportLogModal };
