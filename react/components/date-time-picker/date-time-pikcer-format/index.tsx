import { createElement } from 'react';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import DaysView, { DateViewProps } from 'choerodon-ui/pro/lib/date-picker/DaysView';
// import DateTimesView from 'choerodon-ui/pro/lib/date-picker/DateTimesView';
import WeeksView from 'choerodon-ui/pro/lib/date-picker/WeeksView';
import TimesView from 'choerodon-ui/pro/lib/date-picker/TimesView';
import MonthsView from 'choerodon-ui/pro/lib/date-picker/MonthsView';
import YearsView from 'choerodon-ui/pro/lib/date-picker/YearsView';
import DecadeYearsView from 'choerodon-ui/pro/lib/date-picker/DecadeYearsView';
import { ViewMode } from 'choerodon-ui/pro/lib/date-picker/enum';
import { MINUTE } from '@/constants/DATE_FORMAT';
import DateTimePicker from '../DateTimePicker';
import DateTimesView from './DateTimesView';

export interface DateTimePickerProps extends DatePickerProps {
  /**
   * 日期格式，如 `YYYY-MM-DD HH:mm`
   */
  format: string,
}

const viewComponents: { [x: string]: typeof DaysView } = {
  [ViewMode.decade]: DecadeYearsView,
  [ViewMode.year]: YearsView,
  [ViewMode.month]: MonthsView,
  [ViewMode.date]: DaysView,
  [ViewMode.dateTime]: DateTimesView,
  [ViewMode.week]: WeeksView,
  [ViewMode.time]: TimesView,
};

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

  getPopupContent() {
    const mode = this.getViewMode();
    return createElement(viewComponents[mode], {
      ref: (node: any) => (this.view = node),
      date: this.getSelectedDate(),
      mode: this.getDefaultViewMode(),
      renderer: this.getCellRenderer(mode),
      onSelect: this.handleSelect,
      onSelectedDateChange: this.handleSelectedDateChange,
      onViewModeChange: this.handelViewModeChange,
      isValidDate: this.isValidDate,
      format: this.getDateFormat(),
      step: this.getProp('step') || {},
    } as DateViewProps);
  }
}
