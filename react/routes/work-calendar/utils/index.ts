import moment from 'moment';

function formatDate(date: Date) {
  return date ? moment(date).format('YYYY-MM-DD HH:mm:ss') : date;
}

function formatIssueTime(dateString: string) {
  return dateString ? moment(dateString).format('YYYY-MM-DDTHH:mm:ss') : dateString;
}

export {
  formatDate, formatIssueTime,
};
