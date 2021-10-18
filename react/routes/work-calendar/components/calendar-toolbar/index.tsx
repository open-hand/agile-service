import React, {
  useCallback, useMemo, useState,
} from 'react';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import { FuncType } from 'choerodon-ui/pro/lib/button/interface';
import { useWorkCalendarStore } from '@/routes/work-calendar/stores';
import Style from './index.less';

const CalendarToolbar = observer(() => {
  const { mainStore } = useWorkCalendarStore();
  const calendarApi = mainStore.getCalendarRef?.current?.getApi();
  const viewType = mainStore.getCurrentViewType;
  const [currentStart, setCurrentStart] = useState(calendarApi?.view?.currentStart);

  const setDate = () => {
    const start = calendarApi?.view?.currentStart;
    setCurrentStart(start);
  };

  const handleGoToday = useCallback(() => {
    calendarApi?.today();
    setDate();
  }, [calendarApi]);

  const handleGoPrev = useCallback(() => {
    calendarApi?.prev();
    setDate();
  }, [calendarApi]);

  const handleGoNext = useCallback(() => {
    calendarApi?.next();
    setDate();
  }, [calendarApi]);

  const getViewTime = (formatStr: string, type = 'start') => {
    if (calendarApi && calendarApi.view) {
      const time = type === 'end' ? calendarApi.view?.currentEnd?.valueOf() - 1000 : calendarApi.view?.currentStart;
      return moment(time)?.format(formatStr);
    }
    return '';
  };

  const todayContent = useMemo(() => (
    <div
      role="none"
      onClick={handleGoToday}
      className={Style.goToday}
    >
      <span>返回今日</span>
    </div>
  ), [handleGoToday]);

  const prevContent = useMemo(() => (
    <Button
      onClick={handleGoPrev}
      icon="navigate_before"
      funcType={'flat' as FuncType}
      className={Style.goButton}
    />
  ), [handleGoPrev]);

  const nextContent = useMemo(() => (
    <Button
      onClick={handleGoNext}
      icon="navigate_next"
      funcType={'flat' as FuncType}
      className={`${Style.goNext} ${Style.goButton}`}
    />
  ), [handleGoNext]);

  if (viewType === 'dayGridMonth') {
    return (
      <div className={Style.header}>
        {prevContent}
        <div className={Style.headerTitle}>
          {getViewTime('YYYY年MM月')}
        </div>
        {nextContent}
        <div className={Style.headerButton}>
          {todayContent}
        </div>
      </div>
    );
  }

  return (
    <div className={Style.header}>
      <div className={Style.headerTitle}>
        {getViewTime('YYYY年MM月')}
      </div>
      <div className={Style.headerButton}>
        {todayContent}
        {prevContent}
        {nextContent}
      </div>
      <div>
        {`${getViewTime('YYYY-MM-DD')}-${getViewTime('YYYY-MM-DD', 'end')}`}
      </div>
    </div>
  );
});

export default CalendarToolbar;
