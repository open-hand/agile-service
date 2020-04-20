import React from 'react';
import { observer } from 'mobx-react-lite';
import { DatePicker } from 'choerodon-ui';
import moment from 'moment';

const { RangePicker } = DatePicker;
const getSelectedDate = (value) => {
  if (!value || value.length === 0) {
    return undefined;
  }
  return [moment(value[0]), moment(value[1])];
};
function TimeField({ field, value, onChange }) {
  return (
    <RangePicker
      format="YYYY-MM-DD"
      value={getSelectedDate(value)}
      style={{ margin: '0 5px', width: 205 }}
      onChange={([start, end]) => {
        if (start && end) {
          onChange([
            moment(start).startOf('day').format('YYYY-MM-DD HH:mm:ss'),
            moment(end).startOf('day').format('YYYY-MM-DD HH:mm:ss'),
          ]);
        } else {
          onChange();
        }
      }}
      allowClear
      placeholder={['开始时间', '结束时间']}
      showTime
    />
  );
}
export default observer(TimeField);
