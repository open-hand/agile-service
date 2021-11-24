import moment, { Moment } from 'moment';

export const formatStartDate = (date: string | Moment | undefined, format = false) => {
  if (!date) {
    return undefined;
  }
  if (!format) {
    return moment(date).startOf('day');
  }
  return moment(date).startOf('day').format('YYYY-MM-DD HH:mm:ss');
};

export const formatEndDate = (date: string | Moment, format = false) => {
  if (!format) {
    return moment(date).endOf('day');
  }
  return moment(date).endOf('day').format('YYYY-MM-DD HH:mm:ss');
};
