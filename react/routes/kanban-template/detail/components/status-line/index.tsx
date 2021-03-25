import React from 'react';
import styles from './index.less';

interface StatusLineProps extends React.HTMLAttributes<HTMLDivElement> {
  color: string
}
const StatusLine: React.FC<StatusLineProps> = ({
  className,
  color,
  ...otherProps
}) => (
  <div className={styles.status_line} style={{ backgroundColor: color }} {...otherProps} />
);

export default StatusLine;
