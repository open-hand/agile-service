import React, { useMemo, useImperativeHandle, useCallback } from 'react';
import {
  Form, Select, DataSet, TextField,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import BurnDownComponent from './components/burndown';
import SprintComponent from './components/sprint';
import AccumulationComponent from './components/accumulation';
import PieComponent from './components/pie';
import { RefProps } from '../add-modal';
import EpicBurnDownComponent from './components/epic-burnDown';
import versionBurnDownComponent from './components/version-burnDown';

const { Option } = Select;
export const defaultCharts = new Map([
  ['burndown', { component: BurnDownComponent, name: '燃尽图' }],
  ['sprint', { component: SprintComponent, name: '冲刺报告图' }],
  ['accumulation', { component: AccumulationComponent, name: '累计流量图' }],
  ['pie', { component: PieComponent, name: '统计图' }],
  ['epicBurnDown', { component: EpicBurnDownComponent, name: '史诗燃尽图' }],
  ['versionBu rnDown', { component: versionBurnDownComponent, name: '版本燃尽图' }],
]);
type GetOptionalCharts = () => Map<string, { component: React.FC<any>, name: string }>

let getOptionalCharts: GetOptionalCharts = () => defaultCharts;

export function setGetOptionalCharts(newGetOptionalCharts: GetOptionalCharts) {
  getOptionalCharts = newGetOptionalCharts;
}

interface Props {
  innerRef: React.MutableRefObject<RefProps>
}
const AddChart: React.FC<Props> = ({ innerRef }) => {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
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
  const handleSubmit = useCallback(async () => {
    if (dataSet.validate()) {
      return 'data';
    }
    return false;
  }, [dataSet]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  const optionalCharts = getOptionalCharts();
  const ChartComponent = optionalCharts.get(dataSet.current?.get('chart'))?.component;
  return (
    <>
      <Form dataSet={dataSet} style={{ width: 512 }}>
        <TextField name="title" />
        <Select name="chart">
          {[...optionalCharts.entries()].map(([key, { name }]) => <Option value={key}>{name}</Option>)}
        </Select>
      </Form>
      {ChartComponent && <ChartComponent />}
    </>
  );
};
export default observer(AddChart);
