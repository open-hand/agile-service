import React from 'react';
import IterationSpeed from '@/components/charts/iteration-speed';
import useIterationSpeedReport from '@/components/charts/iteration-speed/useIterationSpeedReport';
import { IterationSpeedSearchVO } from '@/routes/project-report/report-page/store';
import { transformIterationSpeedSearch } from '@/routes/project-report/report-page/components/add-chart/components/iteration-speed';

interface Props {
  filter: IterationSpeedSearchVO
  onFinish?: Function
}
const IterationSpeedComponent: React.FC<Props> = ({ filter, onFinish }) => {
  const [props] = useIterationSpeedReport(transformIterationSpeedSearch(filter), onFinish);
  return (
    <div>
      <IterationSpeed
        {...props}
        option={{
          animation: false,
        }}
      />
    </div>
  );
};
export default IterationSpeedComponent;
