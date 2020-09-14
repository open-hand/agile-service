import React from 'react';
import EpicBurnDown from '@/components/charts/epic-burnDown';
import EpicBurnDownSearch from '@/components/charts/epic-burnDown/search';
import useEpicBurnDownReport from '@/components/charts/epic-burnDown/useEpicBurnDownReport';

const EpicBurnDownComponent: React.FC = () => {
  const [searchProps, props] = useEpicBurnDownReport();
  return (
    <div>
      <EpicBurnDownSearch {...searchProps} />
      <EpicBurnDown {...props} />
    </div>
  );
};
export default EpicBurnDownComponent;
