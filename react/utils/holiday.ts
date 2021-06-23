import { Moment } from 'moment';
import { find } from 'lodash';

interface HolidayConfig {

  /** 冲刺自定义设置 */
  sprintSetting: {
    status: 0 | 1
    workDay: string
  }[],
  orgWorkCalendar: {
    /** 周六周日是否工作 */
    saturdayWork: boolean,
    /** 周六周日是否工作 */
    sundayWork: boolean,
    /** 组织自定义设置 */
    timeZoneWorkCalendarDTOS: {
      status: 0 | 1
      workDay: string
    }[],
    /** 法定工作日 */
    workHolidayCalendarDTOS: {
      status: 0 | 1
      holiday: string
    }[],
  }
}
export default function isHoliday({
  sprintSetting,
  orgWorkCalendar,
}: HolidayConfig, date: Moment) {
  const {
    saturdayWork,
    sundayWork,
    timeZoneWorkCalendarDTOS: selectDays,
    workHolidayCalendarDTOS: holidayRefs,
  } = orgWorkCalendar;
  // 判断日期是否是休息日
  // 优先级如下：
  // 冲刺自定义设置
  // 组织自定义设置
  // 法定工作日
  // 周六周日是否工作

  // status为0代表放假，为1代表上班。
  const daySprintSetting = find(sprintSetting, (c) => c.workDay === date.format('YYYY-MM-DD'));
  if (daySprintSetting) {
    if (daySprintSetting.status === 0) {
      return true;
    }
    return false;
  }
  const orgSetting = find(selectDays, (c) => c.workDay === date.format('YYYY-MM-DD'));
  if (orgSetting) {
    if (orgSetting.status === 0) {
      return true;
    }
    return false;
  }
  // 法定节假日
  const holidayConfig = find(holidayRefs, (c) => c.holiday === date.format('YYYY-MM-DD'));
  if (holidayConfig) {
    if (holidayConfig.status === 0) {
      return true;
    }
    return false;
  }
  const isSaturday = date.weekday() === 5;
  const isSunday = date.weekday() === 6;
  if (isSaturday) {
    return !saturdayWork;
  }
  if (isSunday) {
    return !sundayWork;
  }

  return false;
}
