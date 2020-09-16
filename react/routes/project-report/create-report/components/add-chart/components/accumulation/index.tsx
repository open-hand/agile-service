import React from 'react';
import Accumulation from '@/components/charts/accumulation';
import AccumulationSearch from '@/components/charts/accumulation/search';
import useAccumulationReport from '@/components/charts/accumulation/useAccumulationReport';

const AccumulationComponent: React.FC = () => {
  const [searchProps, props] = useAccumulationReport();
  return (
    <div>
      <AccumulationSearch {...searchProps} />
      <Accumulation {...props} />
    </div>
  );
};
export default AccumulationComponent;
