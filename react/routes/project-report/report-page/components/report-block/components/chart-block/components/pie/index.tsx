import React, { useMemo } from 'react';
import Pie from '@/components/charts/pie-chart';
import usePieChartReport from '@/components/charts/pie-chart/usePieChartReport';
import { PieSearchVO } from '@/routes/project-report/report-page/store';
import { transformPieSearch } from '@/routes/project-report/report-page/components/add-chart/components/pie';

interface Props {
  filter: PieSearchVO
}
const PieComponent: React.FC<Props> = ({ filter }) => {
  const config = useMemo(() => transformPieSearch(filter), [filter]);
  const [, props] = usePieChartReport(config);
  return (
    <div>
      <Pie {...props} />
    </div>
  );
};
export default PieComponent;
