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
      <Sprint
        {...props}
        option={{
          animation: false,
          grid: {
            top: 40,
            bottom: 0,
            left: 0,
            right: 30,
            containLabel: true,
          },
        }}
      />
    </div>
  );
};
export default SprintComponent;
