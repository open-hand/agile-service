import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Content, stores } from '@choerodon/boot';
import {
  Select,
} from 'choerodon-ui';
import moment from 'moment';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import WorkCalendar from '@/components/WorkCalendar';

const { AppState } = stores;
const { Option } = Select;

@observer
class WorkCalendarPage extends Component {
  onWorkDateChange = (data) => {
    const { sprintId } = ScrumBoardStore.getWorkDate;
    const year = moment().year();
    if (data.calendarId) {
      ScrumBoardStore.axiosDeleteCalendarData(data.calendarId).then(() => {
        ScrumBoardStore.axiosGetCalendarData(year);
      });
    } else {
      ScrumBoardStore.axiosCreateCalendarData(sprintId, data).then(() => {
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
