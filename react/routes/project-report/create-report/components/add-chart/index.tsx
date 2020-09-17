import React, {
  useMemo, useImperativeHandle, useCallback, useRef,
} from 'react';
import {
  Form, Select, DataSet, TextField,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { BurnDownConfig } from '@/components/charts/burn-down/useBurnDownReport';
import { SprintConfig } from '@/components/charts/sprint/useSprintReport';
import BurnDownComponent from './components/burndown';
import SprintComponent from './components/sprint';
import AccumulationComponent from './components/accumulation';
import PieComponent from './components/pie';
import { RefProps } from '../add-modal';
import EpicBurnDownComponent from './components/epic-burnDown';
import versionBurnDownComponent from './components/version-burnDown';
import { IReportChartBlock } from '../../store';
import IterationSpeedComponent from './components/iteration_speed';

const { Option } = Select;
export const defaultCharts = new Map([
  ['burndown', { component: BurnDownComponent, name: '燃尽图' }],
  ['sprint', { component: SprintComponent, name: '冲刺报告图' }],
  ['accumulation', { component: AccumulationComponent, name: '累计流量图' }],
  ['pie', { component: PieComponent, name: '统计图' }],
  ['epicBurnDown', { component: EpicBurnDownComponent, name: '史诗燃耗图' }],
  ['versionBurnDown', { component: versionBurnDownComponent, name: '版本燃耗图' }],
  ['iterationSpeed', { component: IterationSpeedComponent, name: '迭代速度图' }],
]);
type GetOptionalCharts = () => Map<string, { component: React.FC<any>, name: string }>

let getOptionalCharts: GetOptionalCharts = () => defaultCharts;

export function setGetOptionalCharts(newGetOptionalCharts: GetOptionalCharts) {
  getOptionalCharts = newGetOptionalCharts;
}

interface Props {
  innerRef: React.MutableRefObject<RefProps>
  data?: IReportChartBlock
}
export interface ChartRefProps {
  submit: () => Promise<BurnDownConfig | SprintConfig>
}
const AddChart: React.FC<Props> = ({ innerRef, data: editData }) => {
  const chartRef = useRef<ChartRefProps>({} as ChartRefProps);
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    data: editData ? [{ title: editData.title, chart: editData.chartType }] : undefined,
    fields: [{
      name: 'title',
      label: '图表标题',
      maxLength: 44,
      required: true,
    }, {
      name: 'chart',
      label: '选择图表',
      required: true,
    }],
  }), [editData]);
  const handleSubmit = useCallback(async () => {
    if (dataSet.validate()) {
      const data = dataSet.current?.toData();
      const search = await chartRef.current.submit();
      const block: IReportChartBlock = {
        id: editData?.id || String(Math.random()),
        title: data.title,
        type: 'chart',
        chartType: data.chart,
        data: {
          filter: search,
        },
      };
      return block;
    }
    return false;
  }, [dataSet, editData?.id]);
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
          {[...optionalCharts.entries()].map(([key, { name }]) => <Option key={key} value={key}>{name}</Option>)}
        </Select>
      </Form>
      {ChartComponent && <ChartComponent innerRef={chartRef} data={editData} />}
    </>
  );
};
export default observer(AddChart);
