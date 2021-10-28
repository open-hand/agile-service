import React from 'react';
import { Modal, DataSet } from 'choerodon-ui/pro';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import ExportLog, { IExportProps } from './ExportLog';
import styles from './ExportLog.less';

function openExportLogModal(props: IExportProps) {
  Modal.open({
    maskClosable: false,
    key: Modal.key(),
    title: props.title || '导出',
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
