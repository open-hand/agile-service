import React from 'react';
import { Form, DatePicker } from 'choerodon-ui';
import moment from 'moment';

const FormItem = Form.Item;
function FieldEndTime({ form, field, initialValue }) {
  const { getFieldDecorator, getFieldValue } = form;
  const issueStartValue = getFieldValue('estimatedStartTime');
  return (
    <FormItem key="estimatedEndTime">
      {getFieldDecorator('estimatedEndTime', {
        rules: [{ required: field.required, message: '预计结束时间是必填项' }],
        initialValue,
      })(
        <DatePicker
          label="预计结束时间"
          placeholder="预计结束时间"
          format="YYYY-MM-DD HH:mm:ss"
          showTime
          allowClear={!field.required}
          disabledDate={(endValue) => {
            if (!endValue || !issueStartValue) {
              return false;
            }
            return endValue.valueOf() < moment(issueStartValue);
          }}
        />,
      )}
    </FormItem>
  );
}

export default FieldEndTime;
