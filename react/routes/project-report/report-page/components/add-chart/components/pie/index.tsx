import React from 'react';
import Pie from '@/components/charts/pie-chart';
import PieSearch from '@/components/charts/pie-chart/search';
import usePieChartReport from '@/components/charts/pie-chart/usePieChartReport';

const PieComponent: React.FC = () => {
  const [searchProps, props] = usePieChartReport();
  return (
    <div>
      <PieSearch {...searchProps} />
      <Pie {...props} />
    </div>
  );
};
export default PieComponent;
