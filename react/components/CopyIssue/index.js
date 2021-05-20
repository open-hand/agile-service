import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import CopyIssue from './CopyIssue';
import styles from './index.less';

export const openEditIssueCopyIssue = (props) => {
  Modal.open({
    className: styles.copyIssue,
    key: 'edit-issue-copy-issue',
    title: `复制问题${props.issue.issueNum}`,
    okText: '复制',
    cancelText: '取消',
    children: <CopyIssue
      {...props}
    />,
  });
};
export default CopyIssue;
