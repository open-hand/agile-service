import React from 'react';
import EpicBurndown from '@/components/charts/epic-burnDown';
import useEpicBurndown from '@/components/charts/epic-burnDown/useEpicBurnDownReport';
import { EpicBurndownSearchVO } from '@/routes/project-report/report-page/store';
import { transformEpicBurndownSearch } from '@/routes/project-report/report-page/components/add-chart/components/epic-burnDown';

interface Props {
  filter: EpicBurndownSearchVO
  onFinish?: Function
}
const EpicBurndownComponent: React.FC<Props> = ({ filter, onFinish }) => {
  const [, props] = useEpicBurndown(transformEpicBurndownSearch(filter), onFinish);
  return (
    <div>
      <EpicBurndown
        {...props}
        option={{
          animation: false,
        }}
      />
    </div>
  );
};
export default EpicBurndownComponent;
