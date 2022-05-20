import React, {
  useMemo, useImperativeHandle, useCallback, useRef, useEffect, useState,
} from 'react';
import {
  Form, Select, DataSet, TextField,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { groupBy } from 'lodash';
import OptGroup from 'choerodon-ui/pro/lib/option/OptGroup';
import SelectTeam from '@/components/select/select-team';
import useIsProgram from '@/hooks/useIsProgram';
import useHasDevops from '@/hooks/useHasDevops';
import { customReportApi } from '@/api';
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
import CustomRecordComponent from './components/custom-report';

const { Option } = Select;

export type ChartMap = { component: React.FC<any>, name: string, group: string, available?: () => Promise<boolean> }
export const defaultCharts = new Map<string, ChartMap>([
  ['burn_down_report', { component: BurnDownComponent, name: '燃尽图', group: '敏捷' }],
  ['sprint_report', { component: SprintComponent, name: '冲刺报告图', group: '敏捷' }],
  ['cumulative_flow_diagram', { component: AccumulationComponent, name: '累计流量图', group: '敏捷' }],
  ['pie_chart', { component: PieComponent, name: '统计图', group: '敏捷' }],
  ['epic_burn_down_report', { component: EpicBurnDownComponent, name: '史诗燃耗图', group: '敏捷' }],
  ['version_burn_down_report', { component: versionBurnDownComponent, name: '版本燃耗图', group: '敏捷' }],
  ['velocity_chart', { component: IterationSpeedComponent, name: '迭代速度图', group: '敏捷' }],
  ['version_chart', { component: VersionReportComponent, name: '版本报告图', group: '敏捷' }],
  ['epic_chart', { component: EpicReportComponent, name: '史诗报告图', group: '敏捷' }],
]);
let charts = defaultCharts;
type GetOptionalCharts = () => Promise<Map<string, ChartMap>>

const getOptionalCharts: GetOptionalCharts = async () => {
  const map = new Map();
  const entries = [...charts.entries()];
  for (let i = 0; i < entries.length; i += 1) {
    const c = entries[i];
    let available = true;
    if (c[1].available === undefined) {
      available = true;
    }
    if (typeof c[1].available === 'function') {
      // eslint-disable-next-line no-await-in-loop
      available = await c[1].available();
    }
    if (available) {
      map.set(c[0], c[1]);
    }
  }
  return map;
};

export function addChartsMap(extraCharts: Map<string, ChartMap>) {
  charts = new Map([...charts, ...extraCharts]);
}

interface Props {
  innerRef: React.MutableRefObject<RefProps>
  data?: IReportChartBlock
  linkTo: (url: string) => void
}
export interface ChartRefProps {
  submit: () => Promise<ChartSearchVO | false>
}
const AddChart: React.FC<Props> = ({ innerRef, data: editData, linkTo }) => {
  const initProject = useMemo(() => editData?.chartSearchVO.projectId, [editData?.chartSearchVO.projectId]);
  const initChartCode = useMemo(() => editData?.chartCode, [editData?.chartCode]);
  const [optionalCharts, setOptionalCharts] = useState(new Map());
  const chartRef = useRef<ChartRefProps>({} as ChartRefProps);
  const hasDevops = useHasDevops();
  const { isProgram } = useIsProgram();
  const loadCustomCharts = useCallback(async () => {
    if (!isProgram) {
      const res = await customReportApi.getCustomReports();
      if (res.length) {
        const customChart = new Map<string, ChartMap>();
        res.forEach((item: any) => {
          customChart.set(`custom-${item.id}`, {
            component: (customProp) => <CustomRecordComponent {...customProp || {}} customChartData={item} />,
            name: item.name,
            group: '自定义',
          });
        });
        addChartsMap(customChart);
      }
    }
  }, [isProgram]);
  useEffect(() => {
    (async () => {
      await loadCustomCharts();
      const data = await getOptionalCharts();
      setOptionalCharts(data);
      editData && dataSet.current?.init({ title: editData.title, chart: editData.chartCode, subProjectId: editData.chartSearchVO.projectId });
    })();
  }, []);
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
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
  }), [isProgram]);
  const projectChanged = dataSet.current?.get('subProjectId') !== initProject;
  const codeChanged = dataSet.current?.get('chart') !== initChartCode;
  const ignoreSearchVO = projectChanged || codeChanged;
  const handleSubmit = useCallback(async () => {
    const search = await chartRef.current.submit();
    if (await dataSet.validate() && search !== false) {
      const data = dataSet.current?.toData();
      const block: IReportChartBlock = {
        key: String(Math.random()),
        title: data.title,
        type: 'chart',
        collapse: false,
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

  const chart = dataSet.current?.get('chart');
  const subProjectId = dataSet.current?.get('subProjectId');
  const ChartComponent = optionalCharts.get(chart)?.component;
  const isSubProjectChart = isProgram && [...defaultCharts.keys()].includes(chart);
  const optionGroups = groupBy([...optionalCharts.entries()].filter((([key, { group }]) => (hasDevops ? true : group !== '质量'))).map(([key, { name, group }]) => ({ key, name, group })), 'group');
  const data = useMemo(() => (ignoreSearchVO ? {
    ...editData,
    chartSearchVO: {
      projectId: dataSet.current?.get('subProjectId'),
    },
  } : editData), [dataSet, editData, ignoreSearchVO]);
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
      <div className="c7n-pro-form-float">
        {(ChartComponent && (isSubProjectChart ? subProjectId : true)) ? (
          <ChartComponent
            key={subProjectId}
            innerRef={chartRef}
            data={data}
            projectId={isProgram ? subProjectId : undefined}
            isProgram={isProgram}
            linkTo={linkTo}
          />
        ) : null}
      </div>
    </>
  );
};
export default observer(AddChart);
