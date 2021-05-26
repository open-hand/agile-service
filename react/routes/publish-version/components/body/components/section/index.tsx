import React from 'react';
import classnames from 'classnames';
import styles from './index.less';

interface Props {
  title?: React.ReactNode
  titleRight?: React.ReactNode
  border?: boolean /** @default true */
  className?: string
  titleClassName?: string
  bodyClassName?: string
}
const PublishVersionSection: React.FC<Props> = ({
  children, title, titleRight, className, border, titleClassName, bodyClassName,
}) => (
  <div className={classnames(styles.section, className)}>
    <span className={classnames(styles.section_title, titleClassName)} style={{ display: !title && !titleRight ? 'none' : undefined }}>
      {title}
      {titleRight}
    </span>
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
