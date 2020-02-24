import React, { Fragment } from 'react';
import { Form } from 'choerodon-ui';
import SelectNumber from '@/components/SelectNumber';

const FormItem = Form.Item;
function WSJF({ getFieldDecorator }) {
  return (
    <Fragment>
      <FormItem key="userBusinessValue">
        {getFieldDecorator('userBusinessValue', {
        })(
          <SelectNumber label="用户/业务价值" selectNumbers={['1', '2', '3', '5', '8', '13', '20']} />,
        )}
      </FormItem>
      <FormItem key="timeCriticality">
        {getFieldDecorator('timeCriticality', {
        })(
          <SelectNumber label="时间紧迫性" selectNumbers={['1', '2', '3', '5', '8', '13', '20']} />,
        )}
      </FormItem>
      <FormItem key="rrOeValue">
        {getFieldDecorator('rrOeValue', {
        })(
          <SelectNumber label="降低风险|促成机会" selectNumbers={['1', '2', '3', '5', '8', '13', '20']} />,
        )}
      </FormItem>
      <FormItem key="jobSize">
        {getFieldDecorator('jobSize', {
        })(
          <SelectNumber label="工作规模" selectNumbers={['1', '2', '3', '5', '8', '13', '20']} />,
        )}
      </FormItem>
    </Fragment>
  );
}

export default WSJF;
