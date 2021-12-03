import { IssueValueHook } from '../interface';
import { DATETIME, FORMAT_FIELDS } from '@/constants/DATE_FORMAT';
import { formatMinute } from '@/utils/formatDate';

// 提交前预定义时间字段需转成‘YYYY-MM-DD HH:mm:ss’格式
const dealWithFormatPredefinedDateFields: IssueValueHook = (values) => {
  FORMAT_FIELDS.forEach((key) => {
    if (values[key]) {
      // eslint-disable-next-line no-param-reassign
      values[key] = formatMinute({ value: values[key], format: DATETIME });
    }
  });
  return values;
};
export default dealWithFormatPredefinedDateFields;
