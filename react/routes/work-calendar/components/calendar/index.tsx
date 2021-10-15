import React, {
  useCallback,
  useEffect, useMemo, useRef, useState,
} from 'react';
import FullCalendar from '@fullcalendar/react';
import { Tooltip } from 'choerodon-ui/pro';
import { CalendarApi } from '@fullcalendar/common';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import interactionPlugin from '@fullcalendar/interaction';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import { xorBy } from 'lodash';
import moment from 'moment';
import { useWorkCalendarStore } from '@/routes/work-calendar/stores';
import openCreateIssue from '@/components/create-issue';
import { Issue } from '@/common/types';
import { formatIssueTime, formatDate } from '@/routes/work-calendar/utils';
import { issueApi } from '@/api';
import DetailContainer, { useDetail } from '@/components/detail-container';
import CalendarToolbar from '@/routes/work-calendar/components/calendar-toolbar';
import Style from './index.less';
import UserTag from '@/components/tag/user-tag';

const TIME_LABEL = ['凌晨0点', '凌晨1点', '凌晨2点', '凌晨3点', '凌晨4点', '早上5点', '早上6点', '早上7点', '早上8点', '上午9点', '上午10点', '上午11点', '中午12点', '下午1点', '下午2点', '下午3点', '下午4点', '下午5点', '晚上6点', '晚上7点', '晚上8点', '晚上9点', '晚上10点', '晚上11点', '晚上12点'];

interface CalendarRefPros {
  getApi(): CalendarApi,
}

interface Props {
  calendarRef: React.MutableRefObject<CalendarRefPros>
}

const CalendarContent = observer(() => {
  const {
    mainStore, AppState,
  } = useWorkCalendarStore();

  const calendarRef = useRef<CalendarRefPros>();

  useEffect(() => {
    if (calendarRef.current) {
      // @ts-ignore
      mainStore.setCalendarRef(calendarRef);
    }
  }, [calendarRef.current]);

  const getCalendarApi = useCallback(() => calendarRef.current?.getApi(), [calendarRef.current]);

  const [issueDetailProps] = useDetail();

  const loadIssuesData = useCallback((info, successCallback) => {
    mainStore.loadIssues(info).then((res) => {
      const calendarApi = getCalendarApi();
      const oldResources = calendarApi?.getEventSources() || [];
      const newResources = xorBy(oldResources.concat(res), 'id');
      successCallback(newResources);
    });
  }, [mainStore, getCalendarApi]);

  const renderEventContent = usePersistFn(({ event, view }) => {
    const hours = event?.start?.getHours();
    const timeText = TIME_LABEL[hours] ?? '';
    const timeLabel = timeText?.slice(0, 2) || '';
    const viewType = view?.type;
    const timeSpan = `${timeLabel}${moment(event?.start).format('HH:mm')}`;
    if (viewType === 'dayGridMonth') {
      return (
        <div
          className={Style.monthIssue}
          style={{
            background: event?.backgroundColor,
            borderLeft: `3px solid ${event?.extendedProps?.priorityVO?.colour || 'transparent'}`,
          }}
        >
          <Tooltip title={event.title}>
            <span className={Style.monthIssueSummary}>{event.title}</span>
          </Tooltip>
          <span className={Style.monthIssueStartTime}>{timeSpan}</span>
        </div>
      );
    }
    if (event.allDay) {
      return (
        <div
          className={Style.allDayIssue}
          style={{
            borderLeft: `3px solid ${event?.extendedProps?.priorityVO?.colour || 'transparent'}`,
          }}
        >
          <div className={Style.issueSummary}>{event.title}</div>
        </div>
      );
    }
    return (
      <>
        <div className={Style.timeIssueTimeWrap}>
          <div className={Style.issueStartTime}>{timeSpan}</div>
          <UserTag
            data={event?.extendedProps?.assignee}
            showText={false}
          />
        </div>
        <div className={Style.issueSummary}>{event.title}</div>
      </>
    );
  });

  const renderDayHeaderContent = usePersistFn(({ date, text }) => {
    if (date?.getDay() === 1) {
      return text;
    }
    return text.slice(-2);
  });

  const renderSlotLabelContent = usePersistFn(({ date }) => {
    const hours = date.getHours();
    if (hours === 0) {
      return '';
    }
    return TIME_LABEL[hours] || '';
  });

  const handleEventChange = usePersistFn(({ event, revert }) => {
    const postData = {
      issueId: event.id,
      objectVersionNumber: event.extendedProps?.objectVersionNumber,
      estimatedStartTime: formatDate(event.start),
      estimatedEndTime: formatDate(event.end),
    };
    issueApi.project(event.extendedProps?.projectId).update(postData).catch(() => {
      revert();
    });
  });

  const handleDateSelect = useCallback((selectInfo) => {
    openCreateIssue({
      defaultValues: {
        estimatedStartTime: formatDate(selectInfo?.startStr),
        estimatedEndTime: formatDate(selectInfo?.endStr),
      },
      defaultAssignee: AppState.userInfo,
      projectId: '223894445333270528',
      onCreate: (issue: Issue) => {
        const calendarApi = getCalendarApi();
        if (calendarApi && issue.estimatedStartTime && issue.estimatedEndTime) {
          calendarApi.addEvent({
            ...issue,
            id: issue.issueId,
            title: issue.summary,
            start: formatIssueTime(issue.estimatedStartTime),
            end: formatIssueTime(issue.estimatedEndTime),
            allDay: selectInfo.allDay,
          });
        }
        calendarApi?.unselect(); // clear date selection
      },
    });
  }, [getCalendarApi]);

  const handleEventClick = usePersistFn(({ event }) => {
    issueDetailProps?.open({
      path: 'issue',
      props: {
        issueId: event?.id,
        projectId: event?.extendedProps?.projectId,
      },
    });
  });

  return (
    <>
      <div className={Style.wrap}>
        <CalendarToolbar />
        <FullCalendar
          // @ts-ignore
          ref={calendarRef}
          plugins={[dayGridPlugin, timeGridPlugin, interactionPlugin]}
          headerToolbar={false}
          themeSystem="bootstrap"
          slotEventOverlap={false}
          eventMinHeight={30}
          slotLabelContent={renderSlotLabelContent}
          dayHeaderContent={renderDayHeaderContent}
          // 每周第一天：Sunday=0, Monday=1, Tuesday=2, etc.
          firstDay={1}
          locale="zh-cn"
          initialView="timeGridWeek"
          editable
          selectable
          selectMirror
          dayMaxEvents
          showNonCurrentDates={false}
          events={loadIssuesData} // use the `events` setting to fetch from a feed
          select={handleDateSelect}
          eventContent={renderEventContent} // custom render function
          eventClick={handleEventClick}
          eventChange={handleEventChange}
          eventTextColor="var(--text-color)"
          eventBackgroundColor="#FFE9B6"
          viewClassNames={Style.viewClass}
          slotLabelClassNames={Style.slotLabel}
          slotLaneClassNames={Style.slotLane}
          dayHeaderClassNames={Style.dayHeader}
          allDayClassNames={Style.allDayLabel}
          allDayContent="全天"
          height="100%"
        />
      </div>
      <DetailContainer {...issueDetailProps} />
    </>
  );
});

export default CalendarContent;
