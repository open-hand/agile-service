import React from 'react';
import classnames from 'classnames';
import styles from './index.less';

interface Props{
    title?:React.ReactNode
    border?:boolean /** @default true */
}
const PublishVersionSection:React.FC<Props> = ({ children, title, border }) => (
  <div className={styles.section}>
    <span className={styles.section_title}>{title}</span>
    <div className={classnames(styles.section_body, { [styles.section_body_border]: typeof (border) === 'undefined' || border })}>
      {children}
    </div>
  </div>
);
export default PublishVersionSection;
