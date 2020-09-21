import React from 'react';
import Sprint from '@/components/charts/sprint';
import useSprintReport from '@/components/charts/sprint/useSprintReport';
import { transformSprintSearch } from '@/routes/project-report/report-page/components/add-chart/components/sprint';
import { SprintSearchVO } from '@/routes/project-report/report-page/store';

interface Props {
  filter: SprintSearchVO
}
const SprintComponent: React.FC<Props> = ({ filter }) => {
  const [, props] = useSprintReport(transformSprintSearch(filter));
  return (
    <div>
      <Sprint {...props} />
    </div>
  );
};
export default SprintComponent;
