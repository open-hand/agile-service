import React from 'react';
import styles from './Container.less';
import Header from './Header';

const Container: React.FC = () => (
  <div
    className={styles.container}
  >
    <Header />
  </div>
);

export default Container;
