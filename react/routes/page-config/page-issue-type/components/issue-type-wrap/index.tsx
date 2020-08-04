import React, { ReactElement } from 'react';
import styles from './index.less';

const IssueTypeWrap: React.FC<{ title: string | ReactElement }> = ({ title, children }) => (
  <div className={styles.card}>
    <span className={styles.title}>{title}</span>
    <div className={styles.content}>
      {children}
    </div>
  </div>
);
export default IssueTypeWrap;
