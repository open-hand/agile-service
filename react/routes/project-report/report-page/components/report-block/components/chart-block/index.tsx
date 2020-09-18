import React from 'react';
import { observer } from 'mobx-react-lite';
import { IReportChartBlock, IChartCode } from '@/routes/project-report/report-page/store';
import BurnDownBlock from './components/burndown';
import SprintBlock from './components/sprint';
import AccumulationBlock from './components/accumulation';
import PieBlock from './components/pie';
import VersionBlock from './components/version-report';
import EpicBlock from './components/epic-report';

const ChartMap = new Map<IChartCode, React.FC<any>>([
  ['burn_down_report', BurnDownBlock],
  ['sprint_report', SprintBlock],
  ['cumulative_flow_diagram', AccumulationBlock],
  ['pie_chart', PieBlock],
  ['version_chart', VersionBlock],
  ['epic_chart', EpicBlock],
]);
interface Props {
  data: IReportChartBlock
}
const ChartBlock: React.FC<Props> = ({ data: { chartSearchVO, chartCode } }) => {
  const ChartBlockComponent = ChartMap.get(chartCode);
  return (
    <>
      {ChartBlockComponent && (
        <ChartBlockComponent filter={chartSearchVO} />
      )}
    </>
  );
};
export default observer(ChartBlock);
