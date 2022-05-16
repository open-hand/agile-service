import React from 'react';
import moment, { Moment } from 'moment';
import { IFilterField } from '../filter';
import { getFilterFields, getSearchFields } from '../field-pro/layouts';

export function getFieldElement(field: IFilterField, flat?: boolean): React.ReactNode {
  const {
    code, fieldType, system,
  } = field;
  return getSearchFields([field], { [field.code]: { flat } })[0];
}
function encodeDate(date: string, isTime: boolean): Moment
function encodeDate(date: string[], isTime: boolean): Moment[]
function encodeDate(date: string | string[], isTime: boolean) {
  if (Array.isArray(date)) {
    return date.map((d) => (d ? moment(isTime ? `2000-01-01 ${d}` : d) : d));
  }
  if (date) {
    return moment(isTime ? `2000-01-01 ${date}` : date);
  }
  return date;
}
function decodeDate(date: Moment, isTime: boolean): string
function decodeDate(date: Moment[], isTime: boolean): string[]
function decodeDate(date: Moment | Moment[], isTime: boolean) {
  if (Array.isArray(date)) {
    return date.map((d) => (d ? moment(d).format(isTime ? 'HH:mm:ss' : 'YYYY-MM-DD HH:mm:ss') : d));
  }
  if (date) {
    return moment(date).format(isTime ? 'HH:mm:ss' : 'YYYY-MM-DD HH:mm:ss');
  }
  return date;
}
export { encodeDate, decodeDate };
