import React, { useCallback, useRef } from 'react';
import { Icon, Tooltip } from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import { FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { observer } from 'mobx-react-lite';
import { useSize } from 'ahooks';
import classNames from 'classnames';
import Moment from 'moment';
import { extendMoment } from 'moment-range';
import DateTable from './DateTable';
import styles from './index.less';
import { useCalendarStore } from '../../stores';
import { formatEndDate, formatStartDate } from '../../stores/CalendarSearchDataSet';
import { getIsOrganization } from '@/utils/common';
// @ts-ignore
const moment = extendMoment(Moment);

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
  const widthPerDayRef = useRef();
  const {
    startTime, endTime, searchDs, projectCreationDate,
  } = useCalendarStore();
  const handleBackToToday = useCallback(() => {
    const isBetween = moment().isBetween(moment(startTime), moment(endTime));
    const diff = moment(endTime).diff(moment(startTime), 'days');
    if (isBetween && diff <= 6) { // 如果在时间范围内，时间范围小于6，不用动
      return;
    }
    if (isBetween && diff > 6) { // 滚动到最后
      // @ts-ignore
      dateTableWrapperRef.current!.scrollLeft = (diff - 6) * (widthPerDayRef.current ?? 0);
      return;
    }
    if (!isBetween) { // 如果不在时间范围内，设置时间即可
      // eslint-disable-next-line no-nested-ternary
      searchDs.current?.set('startTime', formatStartDate(getIsOrganization() ? moment().subtract(6, 'days') : (
        moment().subtract(6, 'days').isBefore(moment(projectCreationDate)) ? moment(projectCreationDate) : moment().subtract(6, 'days')
      )));
      searchDs.current?.set('endTime', formatEndDate(moment()));
    }
  }, [endTime, projectCreationDate, searchDs, startTime]);
  return (
    <div className={classNames(styles.calendar, 'c7n-working-hours-calendar-calendar')}>
      <div className={styles.header}>
        <div className={styles.legend}>
          {
            legendMapArr.map((item) => (
              <div className={styles.legendItem} key={item.key}>
                <Tooltip title={item.tooltip} key={item.key}>
                  <span>
                    <Icon type="help_outline" className={styles.icon} />
                    <span className={styles.label}>{item.label}</span>
                  </span>
                </Tooltip>
                <span className={styles.block} style={{ background: item.color }} />
              </div>
            ))
          }
        </div>
        <Button
          funcType={'raised' as FuncType}
          onClick={handleBackToToday}
          className={styles.backToToday}
          style={{
            marginRight: 6,
          }}
        >
          返回今日
        </Button>
      </div>
      <div
        className={styles.body}
        // @ts-ignore
        ref={dateTableWrapperRef}
      >
        <DateTable dateTableWrapperSize={dateTableWrapperSize} widthPerDayRef={widthPerDayRef} />
      </div>
    </div>
  );
};

export default observer(Calendar);
