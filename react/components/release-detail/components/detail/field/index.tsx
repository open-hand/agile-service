import React from 'react';
import styles from './index.less';

interface Props {
  label: string
}
const Field: React.FC<Props> = ({ label, children }) => (
  <div className={styles.field}>
    <div className={styles.label}>{`${label}:`}</div>
    {children}
  </div>
);

export default Field;
