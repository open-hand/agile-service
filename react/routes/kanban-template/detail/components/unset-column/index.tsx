import React from 'react';
import classNames from 'classnames';
import styles from './index.less';
import CardList from '../card-list';
import StatusLine from '../status-line';

interface UnSetColumnProps extends React.HTMLAttributes<HTMLDivElement> {

}
const UnSetColumn: React.FC<UnSetColumnProps> = ({
  className,
  ...otherProps
}) => {
  const draggableId = JSON.stringify({
    type: 'column',
    columnId: 'unset',
  });
  return (
    <div
      className={classNames(styles.unset_column, className)}
      {...otherProps}
    >
      <div className={styles.unset_column__header}>
        <div className={styles.unset_column__statusName}>
          未对应的状态
        </div>
        <StatusLine color="rgba(0, 0, 0, 0.26)" />
      </div>
      <CardList className={styles.unset_column__content} columnId={draggableId} />
    </div>
  );
};

export default UnSetColumn;
