import React from 'react';
import { observer } from 'mobx-react-lite';
import styles from './ChosenOption.less';

const ChosenOption = () => {
  console.log('render：ChosenOption');
  return (
    <div className={styles.chosenOption}>
      chosenOption
    </div>
  );
};

export default observer(ChosenOption);
