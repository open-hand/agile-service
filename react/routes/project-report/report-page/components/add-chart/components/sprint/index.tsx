import React from 'react';
import Sprint from '@/components/charts/sprint';
import SprintSearch from '@/components/charts/sprint/search';
import useSprintReport from '@/components/charts/sprint/useSprintReport';

const SprintComponent: React.FC = () => {
  const [searchProps, props] = useSprintReport();
  return (
    <div>
      <SprintSearch {...searchProps} />
      <Sprint {...props} />
    </div>
  );
};
export default SprintComponent;
