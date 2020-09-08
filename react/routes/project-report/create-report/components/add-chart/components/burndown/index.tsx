import React from 'react';
import BurnDown from '@/components/charts/burn-down';
import BurnDownSearch from '@/components/charts/burn-down/search';
import useBurnDownReport from '@/components/charts/burn-down/useBurnDownReport';

const BurnDownComponent: React.FC = () => {
  const [searchProps, props] = useBurnDownReport();
  return (
    <div>
      <BurnDownSearch {...searchProps} />
      <BurnDown {...props} />
    </div>
  );
};
export default BurnDownComponent;
