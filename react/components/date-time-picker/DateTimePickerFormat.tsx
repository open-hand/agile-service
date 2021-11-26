import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import { MINUTE } from '@/constants/DATE_FORMAT';
import DateTimePicker from './DateTimePicker';

export interface DateTimePickerProps extends DatePickerProps {
  /**
   * 日期格式，如 `YYYY-MM-DD HH:mm`
   */
  format: string,
}

export default class DateTimePickerWithFormat extends DateTimePicker {
  props: DateTimePickerProps;

  static displayName = 'DateTimePickerD';

  static defaultProps = {
    ...DateTimePicker.defaultProps,
  };

  getDateFormat(): string {
    const { format } = this.props;
    return format || MINUTE;
  }
}
