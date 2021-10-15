import React from 'react';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import { FuncType } from 'choerodon-ui/pro/lib/button/interface';
import { useWorkCalendarStore } from '@/routes/work-calendar/stores';
import Style from './index.less';

const CalendarToolbar = observer(() => {
  const { mainStore } = useWorkCalendarStore();
  const calendarApi = mainStore.getCalendarRef?.current?.getApi();

  const handleGoToday = () => {
    calendarApi?.today();
  };

  const handleGoPrev = () => {
    calendarApi?.prev();
  };

  const handleGoNext = () => {
    calendarApi?.next();
  };

  const getViewTime = (formatStr: string, type = 'start') => {
    const timeType = type === 'end' ? 'currentEnd' : 'currentStart';
    if (calendarApi && calendarApi.view) {
      return moment(calendarApi.view[timeType])?.format(formatStr);
    }
    return '';
  };

  return (
    <div className={Style.header}>
      <div className={Style.headerTitle}>
        {getViewTime('YYYY年MM月')}
      </div>
      <div className={Style.headerButton}>
        <div
          role="none"
          onClick={handleGoToday}
          className={Style.goToday}
        >
          <span>返回今日</span>
        </div>
        <Button
          onClick={handleGoPrev}
          icon="navigate_before"
          funcType={'flat' as FuncType}
          className={Style.goButton}
        />
        <Button
          onClick={handleGoNext}
          icon="navigate_next"
          funcType={'flat' as FuncType}
          className={`${Style.goNext} ${Style.goButton}`}
        />
      </div>
      <div>
        {`${getViewTime('YYYY-MM-DD')}-${getViewTime('YYYY-MM-DD', 'end')}`}
      </div>
    </div>
  );
});

export default CalendarToolbar;
