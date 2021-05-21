import React from 'react';
import { observer } from 'mobx-react';
import { TimePicker, DatePicker, DateTimePicker } from 'choerodon-ui/pro';
import moment from 'moment';

const getSelectedDate = (field, value) => {
  if (!value || value.length === 0 || (!value[0] && !value[1])) {
    return { start: undefined, end: undefined };
  }
  if (field.fieldType === 'time') {
    return { start: moment(`2000-01-01 ${value[0]}`), end: moment(`2000-01-01 ${value[1]}`) };
  }
  return { start: moment(value[0]), end: moment(value[1]) };
};
const PickerMap = new Map([
  ['datetime', DateTimePicker],
  ['date', DatePicker],
  ['time', TimePicker],
]);
function TimeField({ field, value, onChange }) {
  const Picker = PickerMap.get(field.fieldType);
  let width;
  if (field.fieldType === 'datetime') {
    width = 380;
  } else if (field.fieldType === 'date') {
    width = 265;
  } else if (field.fieldType === 'time') {
    width = 230;
  }
  return (
    <div className="c7n-pro-form-float">
      <Picker
        label={field.name}
        labelLayout="float"
        style={{ width, margin: '6px 0' }}
        placeholder={['开始时间', '结束时间']}
        value={getSelectedDate(field, value)}
        onChange={(range) => {
          const { start, end } = range || {};
          if (start && end) {
            if (field.fieldType === 'time') {
              onChange([
                start.format('HH:mm:ss'),
                end.format('HH:mm:ss'),
              ]);
            } else if (field.fieldType === 'date') {
              onChange([
                start.startOf('day').format('YYYY-MM-DD HH:mm:ss'),
                end.endOf('day').format('YYYY-MM-DD HH:mm:ss'),
              ]);
            } else {
              onChange([
                start.format('YYYY-MM-DD HH:mm:ss'),
                end.format('YYYY-MM-DD HH:mm:ss'),
              ]);
            }
          } else {
            onChange([]);
          }
        }}
        range={['start', 'end']}
      />
    </div>
  );
}
export default observer(TimeField);
