import React from 'react';
import { IReportBlock } from '@/routes/project-report/create-report/store';
import BurnDownBlock from './components/burndown';

const ChartMap = new Map([
  ['burndown', BurnDownBlock],
]);
interface Props {
  data: IReportBlock
}
const ChartBlock: React.FC<Props> = ({ data: { data } }) => {
  // const ChartBlockComponent = ChartMap.get(dataSet.current?.get('chart'));
  const ChartBlockComponent = ChartMap.get(data.chartType);
  return (
    <>
      {ChartBlockComponent && (
        <ChartBlockComponent filter={{
          type: 'issueCount',
          sprintId: '=xFlL48OlIBm6InJtxh7OA6pF-SWg1-_JQGrtR1P3sj4==',
          quickFilter: {
            onlyStory: false,
            onlyMe: true,
            quickFilters: [],
            personalFilters: [],
          },
          restDayShow: false,
        }}
        />
      )}
    </>
  );
};
export default ChartBlock;
