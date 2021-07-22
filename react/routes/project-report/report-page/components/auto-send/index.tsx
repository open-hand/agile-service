import React, { useEffect, useCallback } from 'react';
import {
  Modal, DataSet, Form, Select, TimePicker, DateTimePicker, CheckBox,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import { useCreation } from 'ahooks';

interface AutoSendModalProps {
  modal?: IModalProps,
}
const { Option } = Select;
const weekOptions = Array(7).fill(0).map((item, index) => ({
  value: index + 1,
  text: index + 1,
}));
const monthOptions = Array(31).fill(0).map((item, index) => ({
  value: index + 1,
  text: index + 1,
}));
const getOptionsByUnit = (unit: string) => {
  switch (unit) {
    case 'week': {
      return weekOptions;
    }
    case 'month': {
      return monthOptions;
    }
    default: return [];
  }
};
const AutoSendModal: React.FC<AutoSendModalProps> = (props) => {
  const { modal } = props;

  const dataSet = useCreation(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'unit',
      label: '单位',
      required: true,
    }, {
      name: 'date',
      label: '天',
      required: true,
    }, {
      name: 'time',
      label: '时间',
      required: true,
    }, {
      name: 'includeWeek',
      label: '时间',
    }],
  }), []);
  const handleSubmit = useCallback(async () => {
    if (await dataSet.validate()) {
      console.log(dataSet.toData());
    }
    return false;
  }, [dataSet]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  const unit = dataSet.current?.get('unit');
  const dateVisible = ['week', 'month'].includes(unit);
  return (
    <Form dataSet={dataSet} columns={2}>
      <Select name="unit" colSpan={1}>
        <Option value="day">天</Option>
        <Option value="week">周</Option>
        <Option value="month">月</Option>
        <Option value="once">仅一次</Option>
      </Select>
      {dateVisible ? (
        <Select name="date" colSpan={1}>
          {getOptionsByUnit(unit).map(({ value, text }) => <Option value={value}>{text}</Option>)}
        </Select>
      ) : null}
      {unit === 'once' ? <DateTimePicker name="time" colSpan={1} /> : <TimePicker name="time" colSpan={1} />}
      {unit === 'month' ? <CheckBox name="includeWeek">包括周六，周日</CheckBox> : null}
    </Form>
  );
};
const ObserverAutoSendModal = observer(AutoSendModal);
const openAutoSendModal = (props: AutoSendModalProps) => {
  Modal.open({
    key: 'AutoSendModal',
    title: '定时发报',
    drawer: true,
    children: <ObserverAutoSendModal {...props} />,
  });
};
export default openAutoSendModal;
