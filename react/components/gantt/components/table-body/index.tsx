import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import Context from '../../context';
import styles from './index.less';

const TableBody: React.FC = () => {
  const { store } = useContext(Context);

  return (
    <div
      className={styles.scrollable}
      style={{
        width: store.tableWidth,
        height: store.bodyScrollHeight,
      }}
    >
      TableBody
    </div>
  );
};
export default observer(TableBody);
