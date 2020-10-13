import React from 'react';
import Accumulation from '@/components/charts/accumulation';
import useAccumulationReport from '@/components/charts/accumulation/useAccumulationReport';
import { AccumulationSearchVO } from '@/routes/project-report/report-page/store';

import { transformAccumulationSearch } from '@/routes/project-report/report-page/components/add-chart/components/accumulation';

interface Props {
  filter: AccumulationSearchVO
  onFinish?: Function
}
const AccumulationComponent: React.FC<Props> = ({ filter, onFinish }) => {
  const [, props] = useAccumulationReport(transformAccumulationSearch(filter), onFinish);
  return (
    <div>
      <Accumulation {...props} animation={false} />
    </div>
  );
};
export default AccumulationComponent;
