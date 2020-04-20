import React from 'react';
import { observer } from 'mobx-react-lite';
import { DatePicker, Tooltip } from 'choerodon-ui';
import moment from 'moment';

const { RangePicker } = DatePicker;
const getSelectedDate = (value) => {  
  if (!value || value.length === 0) {
    return undefined;
  }
  return [moment(value[0]), moment(value[1])];
};
function CreateDateField({ field, value, onChange }) {
  return value && value.length > 0 ? (
    <Tooltip title={`创建问题时间范围为${moment(value[0]).format('YYYY-MM-DD')} ~  ${moment(value[1]).format('YYYY-MM-DD')}`}>
      <div>
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
        />
      </div>
    </Tooltip>
  ) : (
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
    />
  );
}
export default observer(CreateDateField);
