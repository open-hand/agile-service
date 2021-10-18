import moment from 'moment';
import { assign } from 'lodash';
import { Issue } from '@/common/types';
import { STATUS_COLOR } from '@/routes/work-calendar/stores/index';

function formatDate(date: Date) {
  return date ? moment(date).format('YYYY-MM-DD HH:mm:ss') : date;
}

function formatIssueTime(dateString: string | undefined) {
  return dateString ? moment(dateString).format('YYYY-MM-DDTHH:mm:ss') : dateString;
}

/**
 * 判断是否是全天的issue
 * @param estimatedStartTime 预计开始时间
 * @param estimatedEndTime 预计结束时间
 * @return boolean
 */
function isAllDay(estimatedStartTime: string | undefined, estimatedEndTime: string | undefined) {
  if (estimatedStartTime && estimatedEndTime) {
    const endTime = moment(estimatedEndTime);
    const startTime = moment(estimatedStartTime);
    return endTime.diff(startTime, 'hours') % 24 === 0 && startTime.hours() === 0 && startTime.minutes() === 0;
  }
  return false;
}

/**
 * 将issue数据转换为组件所需数据
 * @param item issue数据
 */
function formatIssueData(item: Issue) {
  if (!item) {
    return item;
  }
  return assign(item, {
    id: item.issueId,
    start: formatIssueTime(item.estimatedStartTime), // 开始、结束时间需转换为'YYYY-MM-DDTHH:mm:ss'格式
    end: formatIssueTime(item.estimatedEndTime),
    title: item.summary,
    // @ts-ignore
    backgroundColor: STATUS_COLOR[item.statusVO?.type || 'todo'],
    borderColor: item.priorityVO?.colour || 'transparent',
    allDay: isAllDay(item.estimatedStartTime, item.estimatedEndTime),
  });
}

export {
  formatDate, formatIssueTime, isAllDay, formatIssueData,
};
