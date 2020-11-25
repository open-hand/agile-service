import React, { useContext, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import classNames from 'classnames';
import Context from '../../context';
import styles from './index.less';

const Divider: React.FC = () => {
  const { store } = useContext(Context);
  const { tableWidth } = store;
  const handleClick = useCallback(() => {
    store.toggleCollapse();
  }, [store]);
  const left = tableWidth;
  return (
    <div
      role="none"
      className={styles.divider}
      style={{ left }}
      onClick={handleClick}
    >
      <hr />
      <div className={styles['icon-wrapper']}>
        <i
          className={classNames(styles.arrow, { [styles.reverse]: left <= 0 })}
        />
      </div>
    </div>
  );
};
export default observer(Divider);
