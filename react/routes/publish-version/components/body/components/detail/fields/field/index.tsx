import React from 'react';
import classnames from 'classnames';
import { pick } from 'lodash';
import styles from './index.less';

interface Props {
  label: string
}
const Field: React.FC<Props> = ({ label, children }) => {
  let Children = <span className={styles.text}>{children}</span>;
  if (React.isValidElement(children)) {
    const { className } = pick(children.props, 'className');
    Children = React.cloneElement(children, { ...children.props, className: classnames(styles.text, className) });
  }
  return (
    <div className={styles.field}>
      <div className={styles.label}>{`${label}:`}</div>
      {Children}
    </div>
  );
};

export default Field;
