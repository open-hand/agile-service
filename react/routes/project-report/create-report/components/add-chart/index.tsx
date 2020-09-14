import React, { useMemo } from 'react';
import {
  Form, Select, DataSet, TextField,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import BurnDownComponent from './components/burndown';
import SprintComponent from './components/sprint';
import AccumulationComponent from './components/accumulation';
import PieComponent from './components/pie';

const { Option } = Select;
const ChartMap = new Map([
  ['burndown', BurnDownComponent],
  ['sprint', SprintComponent],
  ['accumulation', AccumulationComponent],
  ['pie', PieComponent],
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
        <Select name="chart">
          <Option value="burndown">燃尽图</Option>
          <Option value="sprint">冲刺报告图</Option>
          <Option value="accumulation">累计流量图</Option>
          <Option value="pie">统计图</Option>
        </Select>
      </Form>
      {ChartComponent && <ChartComponent />}
    </>
  );
};
export default observer(AddChart);
