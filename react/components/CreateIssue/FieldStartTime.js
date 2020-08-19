import React from 'react';
import { Form, DatePicker } from 'choerodon-ui';
import moment from 'moment';

const FormItem = Form.Item;
function FieldStartTime({ form, field }) {
  const { getFieldDecorator, getFieldValue } = form;
  // const issueStartValue = getFieldValue('issueStartValue');
  const issueEndValue = '2020-08-19 10:00:00';
  return (
    <FormItem key="issueStartTime">
      {getFieldDecorator('issueStartTime', {
        rules: [{ required: field.required, message: '预计开始时间是必填项' }],
      })(
        <DatePicker
          label="预计开始时间"
          placeholder="预计开始时间"
          format="YYYY-MM-DD HH:mm:ss"
          showTime
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
