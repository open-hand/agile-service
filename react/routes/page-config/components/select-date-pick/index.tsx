import React, {
  forwardRef, useState, useRef, useCallback, useEffect, useMemo,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import classnames from 'classnames';
import DaysView from 'choerodon-ui/pro/lib/date-picker/DaysView';
import TimesView from 'choerodon-ui/pro/lib/date-picker/TimesView';
import DateTimesView from 'choerodon-ui/pro/lib/date-picker/DateTimesView';
import YearsView from 'choerodon-ui/pro/lib/date-picker/YearsView';
import MonthsView from 'choerodon-ui/pro/lib/date-picker/MonthsView';
import moment, { Moment } from 'moment';
import { observer } from 'mobx-react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import styles from './index.less';
/**
 * 阻止冒泡以及事件执行
 * @param e
 */
function stopEvent(e:React.MouseEvent) {
  e.preventDefault();
  e.stopPropagation();
}

interface IBaseComponentProps {
  onChange?: (value: any) => void
  onBlur?: () => void
  value?: any
  style?: React.CSSProperties
}
interface IDatePickerPageOption {
  text: any
  value: string
  render?: (option: IDatePickerPageOption, dateValue: Moment | undefined) => React.ReactNode
}
interface DatePickerPageProps extends IBaseComponentProps {
  dateType: 'date' | 'datetime' | 'time'
  defaultValue?: any
  format?: string
}
@observer
class ObserverDateTimesView extends DateTimesView {
  // @observable mode: 'dateTime' | 'time' | undefined;

}

const DateViews = {
  dateTime: ObserverDateTimesView,
  date: DaysView,
  time: TimesView,
  year: YearsView,
  month: MonthsView,
};

type DateViewsKey = 'dateTime' | 'date' | 'time';
const SelectDatePickDateFormat = {
  date: 'YYYY-MM-DD',
  datetime: 'YYYY-MM-DD HH:mm:ss',
  time: 'HH:mm:ss',
};
const SelectPickDate = forwardRef<any, DatePickerPageProps>(({
  value: propsValue, defaultValue, dateType, onBlur, format: propsFormat = 'YYYY-MM-DD HH:mm:ss', ...otherProps
}, ref) => {
  const innerRef = useRef<Select>();
  const format = useMemo(() => propsFormat || SelectDatePickDateFormat[dateType], [dateType, propsFormat]);
  const [value, setValue] = useState<Moment | undefined>(() => {
    const momentValue = moment(propsValue || defaultValue, ['YYYY-MM-DD HH:mm:ss', 'HH:mm:ss']);
    return momentValue.isValid() ? momentValue : moment();
  });
  const [mode, setMode] = useState(() => {
    if (dateType === 'datetime') {
      return 'dateTime';
    }
    return dateType;
  });
  const [options, setOptions] = useState<IDatePickerPageOption[]>(() => [
    { text: '当前时间', value: 'current' },
    {
      text: '自定义时间',
      value: 'custom',
    }]);
  const [optionValue, setOptionValue] = useState<string | undefined>(() => {
    if (propsValue || defaultValue) {
      return moment(propsValue || defaultValue, ['YYYY-MM-DD HH:mm:ss', 'HH:mm:ss']).isValid() ? 'custom' : propsValue || defaultValue;
    }
    return undefined;
  });
  const [visible, setVisible] = useState<boolean>(false);

  function handleChange(code?: string) {
    if (code === 'current') {
      innerRef.current?.choose(new Record({ meaning: '当前时间', value: 'current' }));
    }
    setOptionValue(code);
  }
  function handleChangeDate(date: Moment) {
    setValue(date);
    // onChange && onChange(date.format('YYYY-MM-DD HH:mm:ss'));
    innerRef.current?.choose(new Record({ meaning: date.format(format), value: date.format(format) }));
  }
  const DateView = DateViews[mode as DateViewsKey];
  const handleBindRef = useCallback((newRef) => {
    if (newRef) {
      ref && Object.assign(ref, { current: newRef });
      Object.assign(innerRef, { current: newRef });
    }
  }, [ref]);
  useEffect(() => {
    setVisible(optionValue === 'custom');
  }, [optionValue]);
  return (
    <Select
      ref={handleBindRef}
      value={optionValue === 'custom' ? value?.format(format) : optionValue}
      primitiveValue={false}
      // @ts-ignore
      valueField="value"
      // @ts-ignore
      textField="meaning"
      {...otherProps}
      // dropdownMatchSelectWidth={false}
      onPopupHiddenChange={(hidden) => {
        hidden && onBlur && onBlur();
        setMode(dateType === 'datetime' ? 'dateTime' : dateType);
      }}
      popupContent={(
        <div
          role="none"
          className={styles.date}
          onClick={stopEvent}
          onMouseDown={stopEvent}
        >
          <ul className={styles.date_options}>
            {options.map((item) => (
              <li
                role="menuitem"
                onKeyDown={() => {}}
                className={classnames(styles.date_options_item, { [styles.date_options_item_active]: item.value === optionValue })}
                onClick={() => handleChange(item.value)}
              >
                {item.render ? item.render(item, value) : item.text}
              </li>
            ))}
          </ul>
          <div className={styles.date_picker}>
            {visible ? (
              <DateView
                date={value || moment()}
                onSelect={(newDate: any) => handleChangeDate(newDate)}
                format={format}
                onViewModeChange={(newMode) => {
                  setMode(newMode);
                }}
                onSelectedDateChange={(date) => { setValue(date); }}
                mode={dateType === 'datetime' ? 'dateTime' : dateType as any}
                step={{}}
              />
            ) : null}
          </div>

        </div>
      )}
    >
      <Select.Option value="current">当前时间</Select.Option>
    </Select>
  );
});

export default SelectPickDate;
