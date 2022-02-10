import React, { useEffect, useMemo, useCallback } from 'react';
import {
  Form, TextField, DataSet, TextArea, Icon, Select,
} from 'choerodon-ui/pro';
import moment from 'moment';
import { Choerodon } from '@choerodon/boot';
import { sprintApi } from '@/api';
import { MAX_LENGTH_SPRINT } from '@/constants/MAX_LENGTH';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import DateTimePicker from '@/components/date-time-picker';

async function sprintNameValidator(value) {
  const isSame = await sprintApi.validate(value);
  return isSame ? '冲刺名称已存在' : true;
}

const { Option } = Select;

export default function CreateSprint({ modal: { handleOk, close, handleCancel }, onCreate }) {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'sprintName', type: 'string', label: '冲刺名称', required: true, validator: sprintNameValidator,
      },
      {
        name: 'duration',
        type: 'string',
        label: '周期',
        defaultValue: '0',
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
        name: 'sprintGoal', type: 'string', label: '冲刺目标', maxLength: 30,
      },
    ],
    events: {
      update: ({
        dataSet: createDataSet, name, value, oldValue,
      }) => {
        const duration = Number(createDataSet.current.get('duration'));
        const startDate = createDataSet.current.get('startDate');
        if ((name === 'startDate' && duration) || (name === 'duration' && startDate)) {
          if (startDate) {
            createDataSet.current?.set('endDate', moment(startDate).add(duration, 'weeks'));
          }
        }
      },
    },
  }), []);
  async function submit() {
    const isValidate = await dataSet.validate();
    if (isValidate) {
      const [values] = dataSet.toData();
      const sprint = await sprintApi.create(values);
      if (!sprint.failed) {
        onCreate(sprint);
        close();
        BacklogStore.setModalOpened(false);
      } else {
        // Choerodon.prompt(sprint.message);
      }
    }
    return false;
  }

  useEffect(() => {
    handleCancel(() => {
      BacklogStore.setModalOpened(false);
    });
  }, [handleCancel]);

  useEffect(() => {
    handleOk(submit);
  }, [handleOk]);

  return (
    <Form dataSet={dataSet}>
      <TextField
        name="sprintName"
        required
        maxLength={MAX_LENGTH_SPRINT}
        valueChangeAction="input"
        placeholder="请输入冲刺名称，您可以以冲刺+时间戳命名"
      />
      <Select name="duration" clearButton={false}>
        <Option value="0">自定义</Option>
        <Option value="1">1周</Option>
        <Option value="2">2周</Option>
        <Option value="4">4周</Option>
      </Select>
      <DateTimePicker name="startDate" showStartTime />
      <DateTimePicker name="endDate" defaultPickerValue={moment().endOf('d')} />
      <TextArea
        rowSpan={2}
        colSpan={2}
        name="sprintGoal"
        placeholder="请输入冲刺目标"
      />
    </Form>
  );
}
export function CreateCurrentPiSprint({
  modal: { handleOk, close, handleCancel }, onCreate, sprints, pi,
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
        name: 'duration',
        type: 'string',
        label: '周期',
        defaultValue: '0',
      },
      {
        name: 'startDate',
        type: 'dateTime',
        label: '开始日期',
        required: true,
        validator: checkDateSame,
      },
      {
        name: 'endDate',
        type: 'dateTime',
        label: '结束日期',
        required: true,
        validator: checkDateSame,
      },
      {
        name: 'sprintGoal', type: 'string', label: '冲刺目标', maxLength: 30,
      },
    ],
    events: {
      update: ({
        dataSet: createDataSet, name, value, oldValue,
      }) => {
        const duration = Number(createDataSet.current.get('duration'));
        const startDate = createDataSet.current.get('startDate');
        if ((name === 'startDate' && duration) || (name === 'duration' && startDate)) {
          if (startDate) {
            createDataSet.current?.set('endDate', moment(startDate).add(duration, 'weeks'));
          }
        }
      },
    },
  }), []);

  const isDisabledOption = useCallback((value) => {
    // 开启日期是当前时间
    const startDate = moment();
    // 使用moment套一下，因为add是会改变原值的
    const endDate = moment(startDate).add(Number(value), 'weeks');
    return !BacklogStore.rangeCanChoose({ startDate, endDate, sprintId: undefined });
  }, []);

  async function submit() {
    const isValidate = await dataSet.validate();
    if (isValidate) {
      const [values] = dataSet.toData();
      const sprint = await sprintApi.createOnCurrentPi({ ...values, piId: pi.id });
      if (!sprint.failed) {
        onCreate(sprint);
        close();
        BacklogStore.setModalOpened(false);
      } else {
        // Choerodon.prompt(sprint.message);
      }
    }
    return false;
  }

  useEffect(() => {
    handleCancel(() => {
      BacklogStore.setModalOpened(false);
    });
  }, [handleCancel]);

  useEffect(() => {
    handleOk(submit);
  }, [handleOk]);

  return (
    <Form dataSet={dataSet}>
      <div>
        <Icon type="info" style={{ marginBottom: '.04rem', marginRight: '.05rem', color: 'rgb(255,0,0,1)' }} />
        创建的冲刺将自动关联当前PI：
        {`${pi.code}-${pi.name}`}
      </div>
      <TextField
        name="sprintName"
        required
        maxLength={MAX_LENGTH_SPRINT}
        valueChangeAction="input"
        placeholder="请输入冲刺名称，您可以以冲刺+时间戳命名"
      />
      <Select name="duration" clearButton={false}>
        <Option value="0">自定义</Option>
        <Option value="1" disabled={isDisabledOption('1')}>1周</Option>
        <Option value="2" disabled={isDisabledOption('2')}>2周</Option>
        <Option value="4" disabled={isDisabledOption('4')}>4周</Option>
      </Select>
      <DateTimePicker
        name="startDate"
        filter={(date) => {
          // 没选结束时间的时候，只判断时间点能不能选
          if (!dataSet.current.get('endDate')) {
            return BacklogStore.dateCanChoose({ date });
          }
          // 选了结束时间之后，判断形成的时间段是否和其他重叠
          return BacklogStore.rangeCanChoose({ startDate: date, endDate: dataSet.current.get('endDate') });
        }}
      />
      <DateTimePicker
        defaultPickerValue={moment().endOf('d')}
        name="endDate"
        filter={(date) => {
          // 没选开始时间的时候，只判断时间点能不能选
          if (!dataSet.current.get('startDate')) {
            return BacklogStore.dateCanChoose({ date });
          }
          // 选了开始时间之后，判断形成的时间段是否和其他重叠
          return BacklogStore.rangeCanChoose({ startDate: dataSet.current.get('startDate'), endDate: date });
        }}
      />
      <TextArea
        name="sprintGoal"
        resize="vertical"
        placeholder="请输入冲刺目标"
      />
    </Form>
  );
}
