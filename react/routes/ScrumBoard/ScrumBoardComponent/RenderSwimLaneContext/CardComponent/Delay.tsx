import React from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import styles from './Delay.less';

interface Props {
  day: number
}
const Delay: React.FC<Props> = ({ day }) => (
  <>
    {
        day > 99 && (
          <Tooltip title={`延期${day}天`}>
            <div className={styles.delay}>
              延期99+
            </div>
          </Tooltip>
        )
      }
    {
        day <= 99 && (
          <div className={day > 0 ? styles.delay : styles.soonDelay}>
              {day > 0 ? `延期${day}天` : '即将到期'}
          </div>
        )
      }
  </>
);

export default Delay;
