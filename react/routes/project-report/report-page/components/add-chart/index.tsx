import React, {
  useMemo, useImperativeHandle, useCallback, useRef,
} from 'react';
import {
  Form, Select, DataSet, TextField,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import SelectTeam from '@/components/select/select-team';
import useIsProgram from '@/hooks/useIsProgram';
import { groupBy } from 'lodash';
import OptGroup from 'choerodon-ui/pro/lib/option/OptGroup';
import useHasDevops from '@/hooks/useHasDevops';
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
import CodeQualityReportComponent from './components/code-quality';
import CodeQualityVaryReportComponent from './components/code-quality-vary';
import ServiceCodeQualityReportComponent from './components/service-code-quality';

const { Option } = Select;
export const defaultCharts = new Map([
  ['burn_down_report', { component: BurnDownComponent, name: '燃尽图', group: '敏捷' }],
  ['sprint_report', { component: SprintComponent, name: '冲刺报告图', group: '敏捷' }],
  ['cumulative_flow_diagram', { component: AccumulationComponent, name: '累计流量图', group: '敏捷' }],
  ['pie_chart', { component: PieComponent, name: '统计图', group: '敏捷' }],
  ['epic_burn_down_report', { component: EpicBurnDownComponent, name: '史诗燃耗图', group: '敏捷' }],
  ['version_burn_down_report', { component: versionBurnDownComponent, name: '版本燃耗图', group: '敏捷' }],
  ['velocity_chart', { component: IterationSpeedComponent, name: '迭代速度图', group: '敏捷' }],
  ['version_chart', { component: VersionReportComponent, name: '版本报告图', group: '敏捷' }],
  ['epic_chart', { component: EpicReportComponent, name: '史诗报告图', group: '敏捷' }],
  ['code_quality', { component: CodeQualityReportComponent, name: '代码质量图', group: '质量' }],
  ['code_quality_vary', { component: CodeQualityVaryReportComponent, name: '代码质量变化图', group: '质量' }],
  ['service_code_quality', { component: ServiceCodeQualityReportComponent, name: '应用服务代码质量图', group: '质量' }],
]);
type GetOptionalCharts = () => Map<string, { component: React.FC<any>, name: string, group: string }>

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
  const hasDevops = useHasDevops();
  const { isProgram } = useIsProgram();
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    data: editData ? [{ title: editData.title, chart: editData.chartCode, subProjectId: editData.chartSearchVO.projectId }] : undefined,
    fields: [{
      name: 'title',
      label: '图表标题',
      maxLength: 44,
      required: true,
    }, {
      name: 'chart',
      label: '选择图表',
      required: true,
    },
    {
      name: 'subProjectId',
      label: '子项目',
      textField: 'projName',
      valueField: 'projectId',
      required: true,
      dynamicProps: ({ dataSet: ds }) => ({
        required: isProgram && [...defaultCharts.keys()].includes(ds.current?.get('chart')),
      }),
    },
    ],
  }), [editData, isProgram]);
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
  const chart = dataSet.current?.get('chart');
  const subProjectId = dataSet.current?.get('subProjectId');
  const ChartComponent = optionalCharts.get(chart)?.component;
  const isSubProjectChart = isProgram && [...defaultCharts.keys()].includes(chart);
  const optionGroups = groupBy([...optionalCharts.entries()].filter((([key, { group }]) => (hasDevops ? true : group !== '质量'))).map(([key, { name, group }]) => ({ key, name, group })), 'group');
  return (
    <>
      <Form dataSet={dataSet} style={{ width: 512 }}>
        <TextField name="title" />
        <Select name="chart">
          {Object.keys(optionGroups).map((group) => (
            <OptGroup label={group}>
              {optionGroups[group].map(({ key, name }) => <Option key={key} value={key}>{name}</Option>)}
            </OptGroup>
          ))}
        </Select>
        {isSubProjectChart && chart && <SelectTeam label="子项目" name="subProjectId" />}
      </Form>
      {(ChartComponent && (isSubProjectChart ? subProjectId : true)) ? (
        <ChartComponent
          key={subProjectId}
          innerRef={chartRef}
          data={editData}
          projectId={isProgram ? subProjectId : undefined}
        />
      ) : null}
    </>
  );
};
export default observer(AddChart);
