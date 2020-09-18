import React from 'react';
import IterationSpeed from '@/components/charts/iteration-speed';
import useIterationSpeedReport from '@/components/charts/iteration-speed/useIterationSpeedReport';
import { IterationSpeedSearchVO } from '@/routes/project-report/report-page/store';
import { transformIterationSpeedSearch } from '@/routes/project-report/report-page/components/add-chart/components/iteration-speed';

interface Props {
  filter: IterationSpeedSearchVO
}
const IterationSpeedComponent: React.FC<Props> = ({ filter }) => {
  const [props] = useIterationSpeedReport(transformIterationSpeedSearch(filter));
  return (
    <div>
      <IterationSpeed {...props} />
    </div>
  );
};
export default IterationSpeedComponent;
