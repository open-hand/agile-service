import React, { useEffect } from 'react';
import { Form } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import SelectTeamSprints from '@/components/select/select-teamSprint';

const FormItem = Form.Item;
function FieldTeam({ form: { getFieldDecorator, getFieldValue, setFieldsValue }, teamProjectIds, field }) {
  useEffect(() => { setFieldsValue({ subProjectSprintId: undefined }); }, [getFieldValue('pi'), getFieldValue('teamProjectIds')]);
  return (
    <>
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
    </>
  );
}

export default observer(FieldTeam);
