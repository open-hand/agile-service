import React from 'react';
import { IReportChartBlock } from '@/routes/project-report/report-page/store';
import BurnDownBlock from './components/burndown';
import SprintBlock from './components/sprint';

const ChartMap = new Map([
  ['burndown', BurnDownBlock],
  ['sprint', SprintBlock],
]);
interface Props {
  data: IReportChartBlock
}
const ChartBlock: React.FC<Props> = ({ data: { data, chartType } }) => {
  const ChartBlockComponent = ChartMap.get(chartType);
  return (
    <>
      {ChartBlockComponent && (
        <ChartBlockComponent filter={data.filter} />
      )}
    </>
  );
};
export default ChartBlock;
