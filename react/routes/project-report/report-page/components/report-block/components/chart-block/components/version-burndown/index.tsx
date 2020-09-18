import React from 'react';
import VersionBurndown from '@/components/charts/version-burnDown';
import useVersionBurndown from '@/components/charts/version-burnDown/useVersionBurnDownReport';
import { VersionBurndownSearchVO } from '@/routes/project-report/report-page/store';
import { transformVersionBurndownSearch } from '@/routes/project-report/report-page/components/add-chart/components/version-burnDown';

interface Props {
  filter: VersionBurndownSearchVO
}
const VersionBurndownComponent: React.FC<Props> = ({ filter }) => {
  const [, props] = useVersionBurndown(transformVersionBurndownSearch(filter));
  return (
    <div>
      <VersionBurndown {...props} />
    </div>
  );
};
export default VersionBurndownComponent;
