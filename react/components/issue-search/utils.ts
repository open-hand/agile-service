/* eslint-disable camelcase */
import {
  isEmpty, isEqual, pick, pickBy, isUndefined,
} from 'lodash';

export const getEmptyValue = (field: any) => (field && field.defaultShow ? undefined : null);
export const getDateValue = (value: [string, string] | null, index: 0 | 1) => {
  if (value) {
    return value[index] === 'Invalid date' ? undefined : value[index];
  }
  return value;
};

export const isFilterSame = (obj: { [key: string]: any }, obj2: { [key: string]: any }): boolean => {
  // 过滤掉 [] null '' 那些不起作用的属性
  const keys1 = Object.keys(obj).filter((k) => !isUndefined(obj[k]));
  const keys2 = Object.keys(obj2).filter((k) => !isUndefined(obj2[k]));
  return isEqual(pick(obj, keys1), pick(obj2, keys2));
};
export const filterInvalidAttribute = (obj: { [key: string]: any }): { [key: string]: any } => pickBy(obj, (i) => !isEmpty(i));
/**
 * 对象扁平化 {a:{b:'v'}}  = >  {b:'v'}
 *
 * @param {*} object
 */
export function flattenObject(object: Object): { [key: string]: any } {
  const result: { [key: string]: any } = {};
  for (const [key, value] of Object.entries(object)) {
    if (value !== undefined) {
      if (Object.prototype.toString.call(value) === '[object Object]') {
        Object.assign(result, flattenObject(value));
      } else {
        result[key] = value;
      }
    }
  }
  const {
    date = [],
    date_hms = [],
    number = [],
    option = [],
    string = [],
    text = [],
  } = result;
  [...date, ...date_hms].forEach((d) => {
    result[d.fieldId] = { isCustom: true, value: [d.startDate, d.endDate] };
  });
  [...number, ...option, ...string, ...text].forEach((d) => {
    result[d.fieldId] = { isCustom: true, value: d.value };
  });

  delete result.date;
  delete result.date_hms;
  delete result.number;
  delete result.option;
  delete result.string;
  delete result.text;
  return result;
}
/**
 * 对象扁平化 {a:{b:'v'}}  = >  {b:'v'}
 *
 * @param {*} object
 */
export function SearchVOToFilter(object: { [key: string]: any }): { [key: string]: any } {
  const result: { [key: string]: any } = {};
  for (const [key, value] of Object.entries(object)) {
    if (Object.prototype.toString.call(value) === '[object Object]') {
      Object.assign(result, SearchVOToFilter(value));
    } else if (key === 'createStartDate' || key === 'createEndDate') {
      if (object.createStartDate && object.createEndDate) {
        result.createDate = [object.createStartDate, object.createEndDate];
      }
    } else if (key === 'updateStartDate' || key === 'updateEndDate') {
      if (object.updateStartDate && object.updateEndDate) {
        result.updateDate = [object.updateStartDate, object.updateEndDate];
      }
    } else {
      result[key] = value;
    }
  }
  const {
    date = [],
    date_hms = [],
    number = [],
    option = [],
    string = [],
    text = [],
  } = result;
  [...date, ...date_hms].forEach((d) => {
    result[d.fieldId] = [d.startDate, d.endDate];
  });
  [...number, ...option, ...string, ...text].forEach((d) => {
    result[d.fieldId] = d.value;
  });

  delete result.date;
  delete result.date_hms;
  delete result.number;
  delete result.option;
  delete result.string;
  delete result.text;
  return result;
}
