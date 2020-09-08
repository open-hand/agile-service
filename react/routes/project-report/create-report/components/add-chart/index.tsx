import React, { useMemo } from 'react';
import {
  Form, Select, DataSet, TextField,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import BurnDownComponent from './components/burndown';

const { Option } = Select;
const ChartMap = new Map([
  ['burndown', BurnDownComponent],
]);
const AddChart: React.FC = () => {
  const dataSet = useMemo(() => new DataSet({
    fields: [{
      name: 'title',
      label: '图表标题',
      required: true,
    }, {
      name: 'chart',
      label: '选择图表',
      required: true,
    }],
  }), []);

  const ChartComponent = ChartMap.get(dataSet.current?.get('chart'));
  return (
    <>
      <Form dataSet={dataSet} style={{ width: 512 }}>
        <TextField name="title" />
        <br />
        <Select name="chart">
          <Option value="burndown">燃尽图</Option>
          <Option value="sprint">冲刺报告图</Option>
        </Select>
      </Form>
      {ChartComponent && <ChartComponent />}
    </>
  );
};
export default observer(AddChart);
