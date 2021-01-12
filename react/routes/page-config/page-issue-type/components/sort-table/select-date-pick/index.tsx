import React, {
  forwardRef, useState, useMemo, useEffect,
} from 'react';
import { Select, DatePicker } from 'choerodon-ui/pro';
import classnames from 'classnames';
import DaysView from 'choerodon-ui/pro/lib/date-picker/DaysView';
import TimesView from 'choerodon-ui/pro/lib/date-picker/TimesView';
import DateTimesView from 'choerodon-ui/pro/lib/date-picker/DateTimesView';

import moment, { Moment } from 'moment';
import { Observer } from 'mobx-react-lite';
import { observer } from 'mobx-react';
import {
  observable, action, computed, toJS,
} from 'mobx';
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
const SelectPickDate = forwardRef<any, DatePickerPageProps>((props, ref) => {
  const [value, setValue] = useState<Moment | undefined>(() => {
    const momentValue = moment(props.value, 'YYYY-MM-DD HH:mm:ss');
    return momentValue.isValid() ? momentValue : moment();
  });
  const [mode, setMode] = useState(() => {
    if (props.dateType === 'datetime') {
      return 'dateTime';
    }
    return props.dateType;
  });
  const [options, setOptions] = useState<IDatePickerPageOption[]>(() => [
    { text: '当前时间', value: 'current' },
    {
      text: '自定义时间',
      value: 'custom',
    }]);
  const [optionValue, setOptionValue] = useState<string | undefined>(() => {
    if (props.value) {
      return moment(props.value, 'YYYY-MM-DD HH:mm:ss').isValid() ? 'custom' : props.value;
    }
    return undefined;
  });
  const [visible, setVisible] = useState<boolean>(false);

  console.log('props', props);
  function handleChange(code?: string) {
    setOptionValue(code);
    if (code === 'custom') {
      setVisible(true);
    } else {
      props.onChange && props.onChange(code);
    }
  }
  function handleChangeDate(date: Moment) {
    setValue(date);
    setVisible(false);
    props.onChange && props.onChange(date.format('YYYY-MM-DD HH:mm:ss'));
  }
  const DateView = DateViews[mode as DateViewsKey];

  return (
    <Select
      ref={ref as any}
      trigger={['click'] as any}
      dropdownMatchSelectWidth={false}
      onPopupHiddenChange={(hidden) => {
        hidden && props.onBlur && props.onBlur();
        setMode(props.dateType === 'datetime' ? 'dateTime' : props.dateType);
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
                  console.log('onViewModeChange', newMode);
                  setMode(newMode);
                }}
                mode={props.dateType === 'datetime' ? 'dateTime' : props.dateType as any}
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
