import React, { useRef } from 'react';
import { Icon, Tooltip } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import { useSize } from 'ahooks';
import DateTable from './DateTable';
import styles from './index.less';

const legendMapArr = [
  {
    label: '未饱和',
    key: 'Unsaturated',
    tooltip: '小于8小时',
    color: '#FFB100',
  },
  {
    label: '饱和',
    key: 'saturated',
    tooltip: '8小时',
    color: '#00BFA5',
  },
  {
    label: '过度饱和',
    key: 'overSaturated',
    tooltip: '大于8小时',
    color: '#FF6939',
  },
];
const Calendar = () => {
  const dateTableWrapperRef = useRef();
  const dateTableWrapperSize = useSize(dateTableWrapperRef);
  return (
    <div className={styles.calendar}>
      <div className={styles.header}>
        <div className={styles.legend}>
          {
            legendMapArr.map((item) => (
              <div className={styles.legendItem} key={item.key}>
                <Icon type="help_outline" className={styles.icon} />
                <Tooltip title={item.tooltip} key={item.key}>
                  <span className={styles.label}>{item.label}</span>
                </Tooltip>
                <span className={styles.block} style={{ background: item.color }} />
              </div>
            ))
          }
        </div>
      </div>
      <div
        className={styles.body}
        // @ts-ignore
        ref={dateTableWrapperRef}
      >
        <DateTable dateTableWrapperSize={dateTableWrapperSize} />
      </div>
    </div>
  );
};

export default observer(Calendar);
