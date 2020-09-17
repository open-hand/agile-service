import React from 'react';
import { observer } from 'mobx-react-lite';
import IterationSpeed from '@/components/charts/iteration-speed';
import IterationSpeedSearch from '@/components/charts/iteration-speed/search';
import useIterationSpeedReport from '@/components/charts/iteration-speed/useIterationSpeedReport';

const IterationSpeedComponent = () => {
  const [props, searchProps] = useIterationSpeedReport();
  return (
    <div>
      <IterationSpeedSearch {...searchProps} />
      <IterationSpeed {...props} />
    </div>
  );
};

export default observer(IterationSpeedComponent);
