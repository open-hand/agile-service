import React from 'react';
import styles from './Delay.less';

interface Props {
  day: number
}
const Delay: React.FC<Props> = ({ day }) => (
  <div className={day > 0 ? styles.delay : styles.soonDelay}>
    {day > 0 ? `延期${Math.ceil(day)}天` : '即将到期'}
  </div>
);

export default Delay;
