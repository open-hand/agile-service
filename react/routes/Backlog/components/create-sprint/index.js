import React, { useEffect, useMemo } from 'react';
import {
  Form, TextField, DataSet, TextArea, DateTimePicker, Icon,
} from 'choerodon-ui/pro';
import moment from 'moment';
import { Choerodon } from '@choerodon/boot';
import SprintApi from '@/api/SprintApi';
import { MAX_LENGTH_SPRINT } from '@/constants/MAX_LENGTH';
import IsInProgramStore from '../../../../stores/common/program/IsInProgramStore';

async function sprintNameValidator(value) {
  const isSame = await SprintApi.validate(value);
  return isSame ? '冲刺名称已存在' : true;
}
export default function CreateSprint({ modal: { handleOk, close }, onCreate }) {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'sprintName', type: 'string', label: '冲刺名称', required: true, validator: sprintNameValidator,
      },
      {
        name: 'startDate',
        type: 'dateTime',
        label: '开始日期',
        dynamicProps: {
          max: ({ record }) => record.get('endDate'),
        },
      },
      {
        name: 'endDate',
        type: 'dateTime',
        label: '结束日期',
        dynamicProps: {
          min: ({ record }) => record.get('startDate'),
        },
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
      />
    </Form>
  );
}
export function CreateCurrentPiSprint({
  modal: { handleOk, close }, onCreate, PiName, sprints, piId,
}) {
  function checkDateSame(value, name, record) {
    const startDate = record.get('startDate');
    const endDate = record.get('endDate');
    if (startDate && endDate && startDate.isSame(endDate)) {
      return `${name === 'endDate' ? '结束' : '开始'}时间与${name === 'endDate' ? '开始' : '结束'}时间相同`;
    }
    return true;
  }
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'sprintName', type: 'string', label: '冲刺名称', required: true, validator: sprintNameValidator,
      },
      {
        name: 'startDate',
        type: 'dateTime',
        label: '开始日期',
        required: true,
        validator: checkDateSame,
        min: moment(IsInProgramStore.piInfo.actualStartDate || IsInProgramStore.piInfo.startDate),
        dynamicProps: {
          max: ({ record }) => {
            const { endDate } = IsInProgramStore.piInfo;
            return record.get('endDate') || moment(endDate, 'YYYY-MM-DD HH:mm:ss');
          },
        },
      },
      {
        name: 'endDate',
        type: 'dateTime',
        label: '结束日期',
        required: true,
        validator: checkDateSame,
        max: moment(IsInProgramStore.piInfo.endDate),
        dynamicProps: {
          min: ({ record }) => {
            const startDate = IsInProgramStore.piInfo.actualStartDate || IsInProgramStore.piInfo.startDate;
            return record.get('startDate') || moment(startDate, 'YYYY-MM-DD HH:mm:ss');
          },
        },
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

      const sprint = await SprintApi.createOnCurrentPi({ ...values, piId });
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
    IsInProgramStore.loadPiInfoAndSprint();
  }, [handleOk]);

  return (
    <Form dataSet={dataSet}>
      <div>
        <Icon type="info" style={{ marginBottom: '.04rem', marginRight: '.05rem', color: 'rgb(255,0,0,1)' }} />
        创建的冲刺将自动关联当前PI：
        {PiName}
      </div>
      <TextField name="sprintName" required maxLength={MAX_LENGTH_SPRINT} />
      <DateTimePicker
        name="startDate"
        filter={(currentDate) => {
          const currentDateFormat = currentDate.format('YYYY-MM-DD HH:mm:ss');
          let isBan = !IsInProgramStore.stopChooseBetween(currentDateFormat);
          if (isBan && dataSet.current.get('endDate')) {
            const minTime = IsInProgramStore.findDateMinRange(dataSet.current.get('endDate').format('YYYY-MM-DD HH:mm:ss'));
            if (moment(currentDateFormat).isBefore(minTime)) {
              isBan = false;
            }
          }
          return isBan;
        }}
      />
      <DateTimePicker
        name="endDate"
        filter={(currentDate) => {
          const currentDateFormat = currentDate.format('YYYY-MM-DD HH:mm:ss');
          let isBan = !IsInProgramStore.stopChooseBetween(currentDateFormat);
          if (isBan && dataSet.current.get('startDate')) {
            const maxTime = IsInProgramStore.findDateMaxRange(dataSet.current.get('startDate').format('YYYY-MM-DD HH:mm:ss'));
            if (moment(currentDateFormat).isAfter(maxTime)) {
              isBan = false;
            }
          }
          return isBan;
        }}
      />
      <TextArea
        rowSpan={2}
        colSpan={2}
        name="sprintGoal"
      />
    </Form>
  );
}
