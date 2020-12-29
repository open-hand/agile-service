import { Button } from 'choerodon-ui';
import {
  Select, Radio, DatePicker, CheckBox, TextField, TextArea, NumberField,
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
import styles from './editor.less';

interface IDatePickerPageOption {
  text: any
  value: string
  render?: (option: IDatePickerPageOption, dateValue: Moment | undefined) => React.ReactNode
}
interface DatePickerPageProps {
  dateType: 'date' | 'datetime' | 'time'
  onChange?: (value: any) => void
  onBlur?: () => void
  value?: any
  style?: React.CSSProperties
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
    <Select
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
function renderEditor({ record }: { record: Record }) {
  const bn = 0;
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
        {fieldOptions.map((item: any) => <Select.Option value={item.id || item.tempKey}>{item.value}</Select.Option>)}
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
          autoQueryConfig={{
            selectedUserIds: [],
            // @ts-ignore
            queryUserRequest: async (userId: number) => (type === 'project' ? userApi.getAllInProject('', undefined, userId) : userApi.getAllInOrg('', undefined, userId)),
          }}
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
