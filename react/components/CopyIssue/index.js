import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import CopyIssue from './CopyIssue';
import styles from './index.less';

export const openEditIssueCopyIssue = (props) => {
  Modal.open({
    className: styles.copyIssue,
    key: 'edit-issue-copy-issue',
    title: `复制工作项${props.issue.issueNum}`,
    okText: '复制',
    cancelText: '取消',
    drawer: true,
    style: {
      width: MODAL_WIDTH.middle,
    },
    children: <CopyIssue
      {...props}
    />,
  });
};
export default CopyIssue;
