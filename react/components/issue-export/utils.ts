import { toJS } from 'mobx';
import moment from 'moment';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { ICustomFieldData } from '@/api';
import { IChosenFieldField } from '@/components/chose-field/types';

const getCustomFieldFilters = (chosenFields: Array<IChosenFieldField>, record: Record, transformSystemFilter: Function) => {
  const customField: ICustomFieldData = {
    option: [],
    date: [],
    date_hms: [],
    number: [],
    string: [],
    text: [],
  };
  const systemFilter = {} as any;
  const dateFormatArr = ['HH:mm:ss', 'YYYY-MM-DD HH:mm:ss', 'YYYY-MM-DD'];
  for (let index = 0; index < chosenFields.length; index += 1) {
    const { fieldType, id, code } = chosenFields[index];
    const value = toJS(record.get(code));

    const dateIndex = ['time', 'datetime', 'date'].indexOf(fieldType!);
    if (dateIndex !== -1) {
      if (Array.isArray(value)) {
        for (let j = 0; j < value.length; j += 1) {
          if (moment.isMoment(value[j])) {
            value[j] = value[j].format(dateFormatArr[dateIndex]);
          }
        }
      }
    }
    if (value === undefined || value === null || value === '') {
      // eslint-disable-next-line no-continue
      continue;
    }
    // 系统字段
    if (!id) {
      systemFilter[code] = value;
      // eslint-disable-next-line no-continue
      continue;
    }
    switch (fieldType) {
      case 'single':
      case 'multiple':
      case 'radio':
      case 'checkbox':
      case 'multiMember':
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
            value: value || null,
          });
        }
        break;
      }
      case 'text': {
        if (value && value.length > 0) {
          customField.text.push({
            fieldId: id,
            value: value || null,
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

  const filter = transformSystemFilter(systemFilter);
  const { otherArgs = {} } = filter;
  filter.otherArgs = otherArgs;
  filter.otherArgs.customField = customField;
  return filter;
};
/**
 * 移除code 的额外前缀
 *（用于导出对dataset的name中的code获取）
 * @param code
 * @param prefix 前缀
 * @param startPos 前缀开始的位置
 */
function removeCodeExtraPrefix(code: string, prefix: string = 'foundation.', startPos: number = 0) {
  const isFoundPrefix = typeof code === 'string' ? code.indexOf(prefix) === startPos : false;
  return isFoundPrefix ? code.slice(startPos, prefix.length) : code;
}

export { getCustomFieldFilters, removeCodeExtraPrefix };
