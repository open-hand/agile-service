import React, { useEffect } from 'react';
import { Form } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import SelectTeamSprints from '@/components/select/select-teamSprint';

const FormItem = Form.Item;
function FieldTeam({ form: { getFieldDecorator, setFieldsValue }, teamProjectIds, field }) {
  return (
    <FormItem key="teamProjectIds">
      {getFieldDecorator('teamProjectIds', {
        rules: [{
          required: field.required,
          message: '请填写故事点',
        }],
      })(
        <SelectFocusLoad
          label="负责的子项目"
          style={{
            width: '100%',
            minWidth: 150,
          }}
          loadWhenMount
          mode="multiple"
          type="sub_project"
          afterLoad={() => {
            setFieldsValue({ teamProjectIds });
          }}
        />,
      )}
    </FormItem>

  );
}

export default observer(FieldTeam);
