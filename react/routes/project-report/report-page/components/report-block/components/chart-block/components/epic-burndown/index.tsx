import React from 'react';
import EpicBurndown from '@/components/charts/epic-burnDown';
import useEpicBurndown from '@/components/charts/epic-burnDown/useEpicBurnDownReport';
import { EpicBurndownSearchVO } from '@/routes/project-report/report-page/store';
import { transformEpicBurndownSearch } from '@/routes/project-report/report-page/components/add-chart/components/epic-burnDown';

interface Props {
  filter: EpicBurndownSearchVO
}
const EpicBurndownComponent: React.FC<Props> = ({ filter }) => {
  const [, props] = useEpicBurndown(transformEpicBurndownSearch(filter));
  return (
    <div>
      <EpicBurndown {...props} />
    </div>
  );
};
export default EpicBurndownComponent;
