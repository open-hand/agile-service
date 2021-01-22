import React, {
  forwardRef, useState, useRef, useCallback,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import classnames from 'classnames';
import DaysView from 'choerodon-ui/pro/lib/date-picker/DaysView';
import TimesView from 'choerodon-ui/pro/lib/date-picker/TimesView';
import DateTimesView from 'choerodon-ui/pro/lib/date-picker/DateTimesView';
import moment, { Moment } from 'moment';
import { observer } from 'mobx-react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import styles from './index.less';

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
}
@observer
class ObserverDateTimesView extends DateTimesView {
  // @observable mode: 'dateTime' | 'time' | undefined;

}

const DateViews = {
  dateTime: ObserverDateTimesView,
  date: DaysView,
  time: TimesView,
};

type DateViewsKey = 'dateTime' | 'date' | 'time';
const dateFormat = {
  date: 'YYYY-MM-DD',
  datetime: 'YYYY-MM-DD HH:mm:ss',
  time: 'HH:mm:ss',
};
const SelectPickDate = forwardRef<any, DatePickerPageProps>(({
  value: propsValue, defaultValue, dateType, onBlur, ...otherProps
}, ref) => {
  const innerRef = useRef<Select>();
  const [value, setValue] = useState<Moment | undefined>(() => {
    const momentValue = moment(propsValue || defaultValue, 'YYYY-MM-DD HH:mm:ss');
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
      return moment(propsValue || defaultValue, 'YYYY-MM-DD HH:mm:ss').isValid() ? 'custom' : propsValue || defaultValue;
    }
    return undefined;
  });
  const [visible, setVisible] = useState<boolean>(false);

  function handleChange(code?: string) {
    setOptionValue(code);
    if (code === 'custom') {
      setVisible(true);
    } else {
      // onChange && onChange(code);
      innerRef.current?.choose(new Record({ meaning: 'custom', value: moment().format(dateFormat[dateType]) }));
    }
  }
  function handleChangeDate(date: Moment) {
    setValue(date);
    setVisible(false);
    // onChange && onChange(date.format('YYYY-MM-DD HH:mm:ss'));
    innerRef.current?.choose(new Record({ meaning: 'current', value: date.format(dateFormat[dateType]) }));
  }
  const DateView = DateViews[mode as DateViewsKey];
  const handleBindRef = useCallback((newRef) => {
    if (newRef) {
      ref && Object.assign(ref, { current: newRef });
      Object.assign(innerRef, { current: newRef });
    }
  }, [ref]);
  return (
    <Select
      ref={handleBindRef}
      value={value?.format(dateFormat[dateType])}
      trigger={['click'] as any}
      {...otherProps}
      dropdownMatchSelectWidth={false}
      onPopupHiddenChange={(hidden) => {
        hidden && onBlur && onBlur();
        setMode(dateType === 'datetime' ? 'dateTime' : dateType);
      }}
      popupContent={(
        <div
          role="none"
          className={styles.date}
          onClick={(e) => e.stopPropagation()}
          onMouseDown={(e) => e.stopPropagation()}
        >

          <ul className={styles.date_options}>
            {options.map((item) => (
              <li
                role="none"
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
                format="YYYY-MM-DD HH:mm:ss"
                onViewModeChange={(newMode) => {
                  setMode(newMode);
                }}
                mode={dateType === 'datetime' ? 'dateTime' : dateType as any}
                step={{}}
              />
            ) : null}
          </div>

        </div>
      )}
    />
  );
});

export default SelectPickDate;
