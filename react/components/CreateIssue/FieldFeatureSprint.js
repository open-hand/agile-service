import React, { useEffect } from 'react';
import { Form } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import SelectTeamSprints from '@/components/select/select-teamSprint';

const FormItem = Form.Item;
function FieldFeatureSprint({ form: { getFieldDecorator, getFieldValue, setFieldsValue }, field }) {
  // useEffect(() => { setFieldsValue({ subProjectSprintId: undefined }); }, [getFieldValue('pi'), getFieldValue('teamProjectIds')]);
  return (
    <FormItem label="冲刺" key="feature-sprint">
      {getFieldDecorator('subProjectSprintId', {
        rules: [{ required: field.required, message: '请选择冲刺' }],
      })(
        <SelectTeamSprints
          label="冲刺"
          mode="multiple"
          labelLayout="float"
          style={{ width: '100%' }}
          allowClear
          showCheckAll={false}
          piId={getFieldValue('pi')}
          teamIds={getFieldValue('teamProjectIds')}
        />,
      )}
    </FormItem>
  );
}

export default observer(FieldFeatureSprint);
