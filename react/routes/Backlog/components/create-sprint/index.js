import React, { useEffect, useMemo } from 'react';
import {
  Form, TextField, DataSet, TextArea, DateTimePicker,
} from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import SprintApi from '@/api/SprintApi';
import { MAX_LENGTH_SPRINT } from '@/constants/MAX_LENGTH';

export default function CreateSprint({ modal: { handleOk, close }, onCreate }) {
  async function sprintNameValidator(value, name, record) {
    const isSame = await SprintApi.validate(value);     
    return isSame ? '冲刺名称已存在' : true;
  }
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'sprintName', type: 'string', label: '冲刺名称', required: true, validator: sprintNameValidator, 
      },
      {
        name: 'startDate', type: 'dateTime', label: '开始日期', max: 'endDate',
      },
      {
        name: 'endDate', type: 'dateTime', label: '结束日期', min: 'startDate',
      },
      {
        name: 'sprintGoal', type: 'string', label: '冲刺目标',
      },
    ],
  }));
  async function submit() {
    const isValidate = await dataSet.validate();
    if (isValidate) {
      const [values] = dataSet.toData();  
      const sprint = await SprintApi.create(values);
      if (!sprint.failed) {
        onCreate(sprint);
        close();
      } else {
        Choerodon.prompt(sprint.message);
      }
    }
    return false;
  }
  useEffect(() => {
    handleOk(submit);
  }, [handleOk]);

  return (
    <Form dataSet={dataSet}>
      <TextField name="sprintName" required maxLength={MAX_LENGTH_SPRINT} />
      <DateTimePicker name="startDate" />
      <DateTimePicker name="endDate" />
      <TextArea
        rowSpan={2}
        colSpan={2}
        name="sprintGoal"
      // placeholder="请输入冲刺目标"
      />
    </Form>
  );
}
