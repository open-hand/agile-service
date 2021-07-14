import React from 'react';
import { observer } from 'mobx-react-lite';
import styles from './Rule.less';

const Rule = () => {
  console.log('render：Rule');
  return (
    <div className={styles.rule}>rule</div>
  );
};

export default observer(Rule);
