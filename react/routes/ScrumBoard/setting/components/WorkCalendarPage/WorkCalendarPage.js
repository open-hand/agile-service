import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Content } from '@choerodon/boot';
import moment from 'moment';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import WorkCalendar from '@/components/WorkCalendar';
import { workCalendarApi } from '@/api';

@observer
class WorkCalendarPage extends Component {
  onWorkDateChange = (data) => {
    const { sprintId } = ScrumBoardStore.getWorkDate;
    const year = moment().year();
    if (data.calendarId) {
      workCalendarApi.delete(data.calendarId).then(() => {
        ScrumBoardStore.axiosGetCalendarData(year);
      });
    } else {
      workCalendarApi.create(sprintId, data).then(() => {
        ScrumBoardStore.axiosGetCalendarData(year);
      });
    }
  };

  render() {
    const {
      saturdayWork,
      sundayWork,
      useHoliday,
      timeZoneWorkCalendarDTOS: selectDays,
      workHolidayCalendarDTOS: holidayRefs,
    } = ScrumBoardStore.getWorkSetting;
    const {
      workCalendarRefVOS: workDates,
      startDate,
      endDate,
    } = ScrumBoardStore.getWorkDate;
    const { selectedDateDisabled } = this.props;
    return (
      <Content
        style={{
          padding: 0,
          height: '100%',
        }}
      >
        <WorkCalendar
          startDate={startDate}
          endDate={endDate}
          mode="ScrumBoard"
          saturdayWork={saturdayWork}
          sundayWork={sundayWork}
          useHoliday={useHoliday}
          selectDays={selectDays}
          holidayRefs={holidayRefs}
          workDates={workDates}
          onWorkDateChange={this.onWorkDateChange}
          selectedDateDisabled={selectedDateDisabled}
        />
      </Content>
    );
  }
}

export default WorkCalendarPage;
