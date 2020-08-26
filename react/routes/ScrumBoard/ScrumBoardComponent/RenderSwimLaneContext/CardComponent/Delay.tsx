import React from 'react';
import styles from './Delay.less';

interface Props {
  day: number
}
const Delay: React.FC<Props> = ({ day }) => (
  <div className={styles.delay}>
    {`延期${day}天`}
  </div>
);

export default Delay;
