import React, { useEffect, useMemo, useState } from 'react';
import {
  Form, TextField, DataSet, TextArea, DateTimePicker,
} from 'choerodon-ui/pro';
import moment from 'moment';
import { Choerodon } from '@choerodon/boot';
import SprintApi from '@/api/SprintApi';
import { MAX_LENGTH_SPRINT } from '@/constants/MAX_LENGTH';
/**
 * 判断在这个时间范围内时间是否可访问
 * @param {*} startDate 
 * @param {*} endDate 
 */
function stopChooseBetween(time, start, end) {
  // const date = time.format('YYYY-MM-DD HH:mm:ss');
  const startDate = moment(start);
  const endDate = moment(end);
  const endDateZero = moment(end).hour(0).minute(0).second(0);
  if (moment(time).isBetween(startDate, endDateZero, null, '[]')) {
    return false;
  } else if (moment(time).isBetween(endDateZero, endDate, null, '[]')) {
    return false;
  }
  return true;
}

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
export function CreateCurrentPiSprint({
  modal: { handleOk, close }, onCreate, PiName, sprints,
}) {
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
        name: 'startDate', type: 'dateTime', label: '开始日期', max: 'endDate', required: true,
      },
      {
        name: 'endDate', type: 'dateTime', label: '结束日期', min: 'startDate', required: true,
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
      const sprint = await SprintApi.createOnCurrentPi(values);
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

  function findDateRange(time) {
    return sprints.find((sprint) => {
      const { startDate, endDate } = sprint;
      if (moment(time).isBetween(startDate, endDate)) {
        return true;
      }
      return false;
    });
  }

  return (
    <Form dataSet={dataSet}>
      <div>
        提示：创建的冲刺将自动关联当前PI：
        {PiName}
      </div>
      <TextField name="sprintName" required maxLength={MAX_LENGTH_SPRINT} />
      <DateTimePicker
        name="startDate"
        filter={(currentDate, selected) => {
          let isBan = true;
          // eslint-disable-next-line no-plusplus
          for (let index = 0; index < sprints.length; index++) {
            const { endDate, startDate } = sprints[index];
            if (!stopChooseBetween(currentDate.format('YYYY-MM-DD HH:mm:ss'), startDate, endDate)) {
              isBan = false;
              break;
            }
            // if (moment(currentDate.format('YYYY-MM-DD HH:mm:ss')).) {
            //   const data = dataSet.current.get('endDate').format('YYYY-MM-DD HH:mm:ss');
             
            // }
          }


          return isBan;
        }}
      />
      <DateTimePicker
        name="endDate"
        filter={(currentDate, selected) => {
          let isBan = true;
          // eslint-disable-next-line no-plusplus
          for (let index = 0; index < sprints.length; index++) {
            const { endDate, startDate } = sprints[index];
            if (!stopChooseBetween(currentDate.format('YYYY-MM-DD HH:mm:ss'), startDate, endDate)) {
              isBan = false; 
              break;
            }
          }
          return isBan;
        }}
      />
      <TextArea
        rowSpan={2}
        colSpan={2}
        name="sprintGoal"
      // placeholder="请输入冲刺目标"
      />
    </Form>
  );
}
