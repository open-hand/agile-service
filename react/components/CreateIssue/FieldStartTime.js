import React from 'react';
import { Form, DatePicker } from 'choerodon-ui';
import moment from 'moment';

const FormItem = Form.Item;
function FieldStartTime({ form, field, initialValue }) {
  const { getFieldDecorator, getFieldValue } = form;
  const issueEndValue = getFieldValue('estimatedEndTime');
  return (
    <FormItem key="estimatedStartTime">
      {getFieldDecorator('estimatedStartTime', {
        rules: [{ required: field.required, message: '预计开始时间是必填项' }],
        initialValue,
      })(
        <DatePicker
          style={{ display: 'block', width: '100%' }}
          label="预计开始时间"
          placeholder="预计开始时间"
          format="YYYY-MM-DD HH:mm:ss"
          showTime={{ defaultValue: moment('00:00:00', 'HH:mm:ss') }}
          allowClear={!field.required}
          disabledDate={(startValue) => {
            if (!startValue || !issueEndValue) {
              return false;
            }
            return startValue.valueOf() > moment(issueEndValue);
          }}
        />,
      )}
    </FormItem>
  );
}

export default FieldStartTime;
