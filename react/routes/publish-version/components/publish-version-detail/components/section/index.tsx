import React, { CSSProperties } from 'react';
import classNames from 'classnames';
import styles from './index.less';

interface Props {
  contentClassName?: string
  style?: CSSProperties
  title: string
  buttons?: JSX.Element | string,
  border?: boolean
}
const Section: React.FC<Props> = ({
  style, children, title, buttons, border, contentClassName,
}) => (
  <div
    className={styles.section}
    style={style}
  >
    <div className={styles.header}>
      <div className={styles.title}>{title}</div>
      <div className={styles.buttons}>{buttons}</div>
    </div>
    <div className={classNames(
      styles.content,
      {
        [styles.bordered]: border,
      },
      contentClassName,
    )}
    >
      {children}
    </div>
  </div>
);

export default Section;
