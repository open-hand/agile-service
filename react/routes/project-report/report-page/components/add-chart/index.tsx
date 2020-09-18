import React, {
  useMemo, useImperativeHandle, useCallback, useRef,
} from 'react';
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
import { IReportChartBlock, ChartSearchVO } from '../../store';
import IterationSpeedComponent from './components/iteration-speed';
import VersionReportComponent from './components/version-report';
import EpicReportComponent from './components/epic-report';

const { Option } = Select;
export const defaultCharts = new Map([
  ['burn_down_report', { component: BurnDownComponent, name: '燃尽图' }],
  ['sprint_report', { component: SprintComponent, name: '冲刺报告图' }],
  ['cumulative_flow_diagram', { component: AccumulationComponent, name: '累计流量图' }],
  ['pie_chart', { component: PieComponent, name: '统计图' }],
  ['epic_burn_down_report', { component: EpicBurnDownComponent, name: '史诗燃耗图' }],
  ['version_burn_down_report', { component: versionBurnDownComponent, name: '版本燃耗图' }],
  ['velocity_chart', { component: IterationSpeedComponent, name: '迭代速度图' }],
  ['versionReport', { component: VersionReportComponent, name: '版本报告图' }],
  ['epicReport', { component: EpicReportComponent, name: '史诗报告图' }],
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
  submit: () => Promise<ChartSearchVO>
}
const AddChart: React.FC<Props> = ({ innerRef, data: editData }) => {
  const chartRef = useRef<ChartRefProps>({} as ChartRefProps);
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    data: editData ? [{ title: editData.title, chart: editData.chartCode }] : undefined,
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
    if (await dataSet.validate()) {
      const data = dataSet.current?.toData();
      const search = await chartRef.current.submit();
      const block: IReportChartBlock = {
        key: String(Math.random()),
        title: data.title,
        type: 'chart',
        chartCode: data.chart,
        chartSearchVO: search,
      };
      return block;
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
          {[...optionalCharts.entries()].map(([key, { name }]) => <Option key={key} value={key}>{name}</Option>)}
        </Select>
      </Form>
      {ChartComponent && <ChartComponent innerRef={chartRef} data={editData} />}
    </>
  );
};
export default observer(AddChart);
