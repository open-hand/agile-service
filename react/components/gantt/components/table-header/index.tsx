import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import Context from '../../context';
import styles from './index.less';

const TableHeader: React.FC = () => {
  const { store } = useContext(Context);

  return (
    <div
      className={styles.chart}
      style={{
        width: store.tableWidth,
      }}
    >
      TableHeader
    </div>
  );
};
export default observer(TableHeader);
