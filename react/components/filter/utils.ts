/* eslint-disable camelcase */
import { find } from 'lodash';
import { IFilter } from '.';
import { IFilterField, ICustomField } from './useFilter';

interface ICustomFieldSearch {
  fieldId: string
  value: any
}
interface IDateFieldSearch {
  fieldId: string,
  startDate: string,
  endDate: string,
}
export interface ISearchVO {
  advancedSearchArgs: {
    issueTypeId?: string[],
    reporterIds?: string[],
    statusId?: string[],
    priorityId?: string[],
  },
  otherArgs: {
    assigneeId?: string[],
    issueIds?: string[],
    component?: string[],
    epic?: string[],
    feature?: string[],
    label?: string[],
    sprint?: string[],
    summary?: string[],
    version?: string[],
    customField?: {
      option: ICustomFieldSearch[],
      date: IDateFieldSearch[],
      date_hms: IDateFieldSearch[],
      number: ICustomFieldSearch[],
      string: ICustomFieldSearch[],
      text: ICustomFieldSearch[],
    },
  },
  searchArgs: {
    createStartDate?: string,
    createEndDate?: string,
    updateStartDate?: string,
    updateEndDate?: string,
    teamProjectIds?: string[]
  },
  quickFilterIds?: string[],
  contents?: string[],
}
function transformSystemFilter(data: IFilter): ISearchVO {
  const {
    issueTypeId,
    assigneeId,
    statusId,
    priorityId,
    issueIds,
    quickFilterIds,
    createDate = [],
    updateDate = [],
    contents,
    component,
    epic,
    feature,
    label,
    reporterIds,
    sprint,
    summary,
    version,
    teamProjectIds,
  } = data;
  return {
    advancedSearchArgs: {
      issueTypeId,
      reporterIds,
      statusId,
      priorityId,
    },
    otherArgs: {
      assigneeId,
      issueIds,
      component,
      epic,
      feature,
      label,
      sprint,
      summary,
      version,
    },
    searchArgs: {
      createStartDate: createDate[0],
      createEndDate: createDate[1],
      updateStartDate: updateDate[0],
      updateEndDate: updateDate[1],
      teamProjectIds,
    },
    quickFilterIds,
    contents,
  };
}
export function filterToSearchVO(filter: IFilter, fields: IFilterField[]): ISearchVO {
  const customField: ISearchVO['otherArgs']['customField'] = {
    option: [],
    date: [],
    date_hms: [],
    number: [],
    string: [],
    text: [],
  };
  const systemFilter: { [key: string]: any } = {};
  Object.keys(filter).forEach((code) => {
    const field = find(fields, { code });

    if (field) {
      const { fieldType, system } = field;
      const value = filter[code];
      if (value === undefined || value === null || value === '') {
        return;
      }

      // 系统字段
      if (system) {
        systemFilter[code] = value;
        return;
      }
      const { id } = field as ICustomField;
      switch (fieldType) {
        case 'single':
        case 'multiple':
        case 'radio':
        case 'checkbox':
        case 'member': {
          const v = Array.isArray(value) ? value : [value];
          if (v.length > 0) {
            customField.option.push({
              fieldId: id,
              value: v,
            });
          }
          break;
        }
        case 'input': {
          if (value && value.length > 0) {
            customField.string.push({
              fieldId: id,
              value,
            });
          }
          break;
        }
        case 'text': {
          if (value && value.length > 0) {
            customField.text.push({
              fieldId: id,
              value,
            });
          }
          break;
        }
        case 'number': {
          customField.number.push({
            fieldId: id,
            value,
          });
          break;
        }
        case 'time':
        case 'datetime':
        case 'date': {
          if (value && value.length > 0) {
            if (fieldType === 'time') {
              customField.date_hms.push({
                fieldId: id,
                startDate: value[0],
                endDate: value[1],
              });
            } else {
              customField.date.push({
                fieldId: id,
                startDate: value[0],
                endDate: value[1],
              });
            }
          }
          break;
        }
        default: break;
      }
    }
  });
  const result = transformSystemFilter(systemFilter);
  result.otherArgs.customField = customField;
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
