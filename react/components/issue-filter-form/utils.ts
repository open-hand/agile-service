import { DataSet } from 'choerodon-ui/pro';
import { isBoolean, isEmpty, isNumber } from 'lodash';
import { toJS } from 'mobx';
import moment from 'moment';
import { IChosenFieldField } from '../chose-field/types';

const dateFormatArr = ['HH:mm:ss', 'YYYY-MM-DD HH:mm:ss', 'YYYY-MM-DD'];

/**
 * 初始化IssueFilterForm 中的字段 value
 * @param field
 * @param dataSet
 */
export function initFieldIssueFilterForm(field: IChosenFieldField, dataSet: DataSet) {
  let values = toJS(field.value);
  if (!isEmpty(values) || isNumber(values) || isBoolean(values)) {
    if (field.fieldType === 'member') {
      values = Array.isArray(values) ? values.map((item) => String(item)) : String(values);
    }
    const dateIndex = ['time', 'datetime', 'date'].indexOf(field.fieldType ?? '');
    if (dateIndex !== -1) {
      values = Array.isArray(values) ? values.map((item) => moment(item, dateFormatArr[dateIndex]))
        : moment(values, dateFormatArr);
    }
    !dataSet.current?.get(field.code) && dataSet.current?.init(field.code, values);
  }
}
