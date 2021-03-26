import React, { useContext } from 'react';
import classNames from 'classnames';
import { observer } from 'mobx-react-lite';
import styles from './index.less';
import CardList from '../card-list';
import StatusLine from '../status-line';
import { Context } from '../..';

interface UnSetColumnProps extends React.HTMLAttributes<HTMLDivElement> {

}
const UnSetColumn: React.FC<UnSetColumnProps> = ({
  className,
  ...otherProps
}) => {
  const { store } = useContext(Context);
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
      <CardList className={styles.unset_column__content} columnId="0" status={store.unsetStatus} />
    </div>
  );
};

export default observer(UnSetColumn);
