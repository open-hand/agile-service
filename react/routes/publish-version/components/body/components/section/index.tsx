import React from 'react';
import ReactDom from 'react-dom';
import classnames from 'classnames';
import styles from './index.less';

interface Props {
  title?: React.ReactNode
  border?: boolean /** @default true */
  className?: string
  bodyClassName?:string
}
const PublishVersionSection: React.FC<Props> = ({
  children, title, className, border, bodyClassName,
}) => (
  <div className={classnames(styles.section, className)}>
    <span className={styles.section_title}>{title}</span>

    {React.Children.map(children, (ch) => (
      <div
        className={classnames(styles.section_body, { [styles.section_body_border]: typeof (border) === 'undefined' || border }, bodyClassName)}
      >
        {ch}
      </div>
    ))}

  </div>
);
export default PublishVersionSection;
