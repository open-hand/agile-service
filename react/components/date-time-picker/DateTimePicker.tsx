/* eslint-disable react/static-property-placement */
import moment, { Moment, isMoment } from 'moment';
import DateTimePicker from 'choerodon-ui/pro/lib/date-time-picker/DateTimePicker';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';

export interface DateTimePickerProps extends DatePickerProps {
  // 默认弹框中的值
  defaultPickerValue?: Moment,
  // 用于创建冲刺中开始日期的时间默认为00:00:00
  showStartTime?: boolean,
}

export default class DateTimePickerWithConfig extends DateTimePicker {
  props: DateTimePickerProps

  static displayName = 'DateTimePickerD';

  static defaultProps = {
    ...DateTimePicker.defaultProps,
  };

  getValidDate(date: Moment): Moment {
    const { min, max } = this;
    const { showStartTime = false } = this.props;
    let newData = date;
    if (min && date.isSameOrBefore(min)) {
      newData = min;
    } else if (max && date.isSameOrAfter(max)) {
      newData = showStartTime ? moment(max).startOf('d') : max;
    }
    return newData;
  }

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
    // 先得到合规时间
    const date = this.getValidDate(defaultPickerValue || moment().startOf('d'));
    // 如果传了默认值，认定是当天结束，暂时这么处理
    return defaultPickerValue ? this.getValidDate(date.endOf('day')) : date;
  }
}
