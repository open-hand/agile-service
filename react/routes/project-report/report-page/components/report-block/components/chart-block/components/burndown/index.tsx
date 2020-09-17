import React from 'react';
import BurnDown from '@/components/charts/burn-down';
import useBurnDownReport from '@/components/charts/burn-down/useBurnDownReport';
import { BurnDownSearchVO } from '@/routes/project-report/report-page/store';
import { transform } from '../../../../../add-chart/components/burndown';

interface Props {
  filter: BurnDownSearchVO
}
const BurnDownComponent: React.FC<Props> = ({ filter }) => {
  const [, props] = useBurnDownReport(transform(filter));
  return (
    <div>
      <BurnDown {...props} />
    </div>
  );
};
export default BurnDownComponent;
