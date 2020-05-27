import React from 'react';
import { Form } from 'choerodon-ui';
import SelectFocusLoad from '@/components/SelectFocusLoad';

const FormItem = Form.Item;
function FieldTeam({ form: { getFieldDecorator } }) {
  return (
    <FormItem key="teamProjectIds">
      {getFieldDecorator('teamProjectIds', {})(
        <SelectFocusLoad
          label="负责的子项目"
          style={{
            width: '100%',
            minWidth: 150,
          }}
          loadWhenMount
          mode="multiple"
          type="sub_project"
        />,
      )}
    </FormItem>
  );
}

export default FieldTeam;
