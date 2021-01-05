import React from 'react';
import { observer } from 'mobx-react-lite';
import styles from './Finish.less';

const Finish = () => (
  <div className={styles.finish}>
    第三步
  </div>
);

export default observer(Finish);
