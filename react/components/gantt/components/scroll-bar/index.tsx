import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import classNames from 'classnames';
import Context from '../../context';
import styles from './index.less';

const ScrollBar: React.FC = () => {
  const { store } = useContext(Context);
  const { tableWidth, viewWidth } = store;
  const left = tableWidth;
  return (
    <div
      role="none"
      className={styles.scroll_bar}
      style={{ left, width: viewWidth }}
    >
      <div
        className={styles.thumb}
        style={{
          width: 100,
          left: 100,
        }}
      />
    </div>
  );
};
export default observer(ScrollBar);
