import React from 'react';
import { observer } from 'mobx-react-lite';
import { IReportChartBlock, IChartCode } from '@/routes/project-report/report-page/store';
import BurnDownBlock from './components/burndown';
import SprintBlock from './components/sprint';

const ChartMap = new Map<IChartCode, React.FC<any>>([
  ['burn_down_report', BurnDownBlock],
  ['sprint_report', SprintBlock],
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
