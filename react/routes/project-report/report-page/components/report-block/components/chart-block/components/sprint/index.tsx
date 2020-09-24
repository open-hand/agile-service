import React from 'react';
import Sprint from '@/components/charts/sprint';
import SprintSearch from '@/components/charts/sprint/search';
import useSprintReport from '@/components/charts/sprint/useSprintReport';
import { transformSprintSearch } from '@/routes/project-report/report-page/components/add-chart/components/sprint';
import { SprintSearchVO } from '@/routes/project-report/report-page/store';

interface Props {
  filter: SprintSearchVO
  onFinish?: Function
}
const SprintComponent: React.FC<Props> = ({ filter, onFinish }) => {
  const [searchProps, props] = useSprintReport(transformSprintSearch(filter), onFinish);
  return (
    <div>
      <div style={{ display: 'none' }}>
        <SprintSearch {...searchProps} />
      </div>
      <Sprint {...props} animation={false} />
    </div>
  );
};
export default SprintComponent;
