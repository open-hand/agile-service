import { Button } from 'choerodon-ui';
import {
  Select as SelectPro, Radio, DatePicker, CheckBox, TextField, TextArea, NumberField,
} from 'choerodon-ui/pro/lib';
import DaysView from 'choerodon-ui/pro/lib/date-picker/DaysView';
import moment, { Moment } from 'moment';
import classnames from 'classnames';
import React, {
  forwardRef, useCallback, useMemo, useState,
} from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import SelectUser from '@/components/select/select-user';
import { getMenuType } from '@/utils/common';
import { userApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import styles from './editor.less';

interface IDatePickerPageOption {
  text: any
  value: string
  render?: (option: IDatePickerPageOption, dateValue: Moment | undefined) => React.ReactNode
}
interface IBaseComponentProps {
  onChange?: (value: any) => void
  onBlur?: () => void
  value?: any
  style?: React.CSSProperties
}
interface DatePickerPageProps extends IBaseComponentProps {
  dateType: 'date' | 'datetime' | 'time'
}
const DatePickerPage = forwardRef<any, DatePickerPageProps>((props, ref) => {
  const [value, setValue] = useState<Moment | undefined>(() => props.value || moment());
  const [options, setOptions] = useState<IDatePickerPageOption[]>(() => [
    { text: '当前时间', value: 'current' },
    {
      text: '自定义时间',
      value: 'custom',
    }]);
  const [optionValue, setOptionValue] = useState<string | undefined>();
  const [visible, setVisible] = useState<boolean>(false);
  // function handleChange(v: any) {
  //   if (moment.isMoment(v)) {
  //     setValue(v);
  //     setVisible(false);
  //   }
  // }
  console.log('props', props);
  function handleChange(code?: string) {
    setOptionValue(code);
    if (code === 'custom') {
      setVisible(true);
    } else {
      props.onChange && props.onChange(moment());
    }
  }
  function handleChangeDate(date: Moment) {
    setValue(date);
    setVisible(false);
    props.onChange && props.onChange(date);
  }
  return (
    <SelectPro
      ref={ref as any}
      trigger={['click'] as any}
      dropdownMatchSelectWidth={false}
      onPopupHiddenChange={(hidden) => {
        hidden && props.onBlur && props.onBlur();
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
              <DaysView
                date={value || moment()}
                onSelect={(newDate) => handleChangeDate(newDate)}
                format="YYYY-MM-DD"
                mode={'date' as any}
                step={{}}
              />
            ) : null}
          </div>

        </div>
      )}
    />
  );
});

const Select = forwardRef<any, IBaseComponentProps & Partial<SelectProps & { children: any }>>(({
  children, onBlur, multiple, ...otherProps
}, ref) => (
  <SelectPro
    ref={ref}
    {...otherProps}
    trigger={multiple ? ['click'] as any : undefined}
    multiple={multiple}
    onChange={(newValue) => {
      console.log('newValue...', newValue, !!(!multiple && otherProps.onChange));
      otherProps.onChange && otherProps.onChange(newValue);
    }}
    onPopupHiddenChange={(hidden) => {
      console.log('blur', hidden, onBlur);
      hidden && onBlur && onBlur();
    }}
  >
    {children}
  </SelectPro>
));

function renderEditor({ record, defaultValue, dataRef }: { record: Record, defaultValue: any, dataRef?: { current: any } }) {
  const fieldType = record.get('fieldType');
  if (['date', 'time', 'datetime'].includes(fieldType)) {
    return <DatePickerPage dateType={fieldType} />;
  }
  if (['checkbox', 'multiple', 'radio', 'single'].includes(fieldType)) {
    const fieldOptions = record.get('fieldOptions') || [];
    return (
      <Select
        multiple={['checkbox', 'multiple'].includes(fieldType)}
      >
        {fieldOptions.map((item: any) => <SelectPro.Option value={item.id || item.tempKey}>{item.value}</SelectPro.Option>)}
      </Select>
    );
  }
  switch (fieldType) {
    case 'input':
      return <TextField maxLength={100} />;
    case 'text':
      return <TextArea rows={3} maxLength={255} />;
    case 'member':
    {
      const type = getMenuType();
      return (
        <SelectUser
          extraOptions={defaultValue ? [defaultValue] : undefined}
            // autoQueryConfig={{
            //   selectedUserIds: defaultValue || [],
            //   // @ts-ignore
            //   queryUserRequest: async (userId: number) => (type === 'project' ? userApi.getAllInProject('', undefined, userId) : userApi.getAllInOrg('', undefined, userId)),
            // }}
          dataRef={dataRef}
          request={({ filter, page }) => (type === 'project' ? userApi.getAllInProject(filter, page) : userApi.getAllInOrg(filter, page))}
        />
      );
    }
    case 'number': {
      const extraConfig = record.get('extraConfig');
      return <NumberField step={extraConfig ? 0.1 : 1} />;
    }
    default:
      break;
  }
  return <TextField />;
}

export default renderEditor;
