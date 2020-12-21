/* eslint-disable react/static-property-placement */
import moment, { Moment, isMoment } from 'moment';
import DateTimePicker from 'choerodon-ui/pro/lib/date-time-picker/DateTimePicker';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';

interface DateTimePickerProps extends DatePickerProps {
  // 默认弹框中的值
  defaultPickerValue?: Moment,
}

export default class DateTimePickerWithConfig extends DateTimePicker {
  props: DateTimePickerProps

  static displayName = 'DateTimePickerD';

  static defaultProps = {
    ...DateTimePicker.defaultProps,
  };

  getSelectedDate(): Moment {
    const {
      range, multiple, rangeTarget, rangeValue,
    } = this;
    const selectedDate = this.selectedDate
      || (range && !multiple && rangeTarget !== undefined && rangeValue && rangeValue[rangeTarget])
      || (!multiple && this.getValue());
    if (isMoment(selectedDate) && selectedDate.isValid()) {
      return selectedDate.clone();
    }
    const { defaultPickerValue } = this.props;
    return this.getValidDate(defaultPickerValue || moment().startOf('d'));
  }
}
