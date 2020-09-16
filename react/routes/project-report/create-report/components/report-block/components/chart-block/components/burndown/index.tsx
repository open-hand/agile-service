import React from 'react';
import BurnDown from '@/components/charts/burn-down';
import useBurnDownReport, { BurnDownConfig } from '@/components/charts/burn-down/useBurnDownReport';

interface Props {
  filter: BurnDownConfig
}
const BurnDownComponent: React.FC<Props> = ({ filter }) => {
  const [, props] = useBurnDownReport(filter);
  return (
    <div>
      <BurnDown {...props} />
    </div>
  );
};
export default BurnDownComponent;
