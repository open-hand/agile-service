import React, { Component } from 'react';
import { Modal } from 'choerodon-ui/pro';
import CopyIssue from './CopyIssue';
import styles from './index.less';

export const openEditIssueCopyIssue = (props) => {
  const { issueNum } = props;
  console.log('issueNum...');
  Modal.open({
    className: styles.copyIssue,
    key: 'edit-issue-copy-issue',
    title: `复制问题${issueNum}`,
    okText: '复制',
    cancelText: '取消',
    children: <CopyIssue
      {...props}
    />,
  });
};
export default CopyIssue;
