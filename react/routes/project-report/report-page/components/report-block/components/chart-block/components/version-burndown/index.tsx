import React from 'react';
import VersionBurndown from '@/components/charts/version-burnDown';
import useVersionBurndown from '@/components/charts/version-burnDown/useVersionBurnDownReport';
import { VersionBurndownSearchVO } from '@/routes/project-report/report-page/store';
import { transformVersionBurndownSearch } from '@/routes/project-report/report-page/components/add-chart/components/version-burnDown';

interface Props {
  filter: VersionBurndownSearchVO
  onFinish?: Function
}
const VersionBurndownComponent: React.FC<Props> = ({ filter }, onFinish) => {
  const [, props] = useVersionBurndown(transformVersionBurndownSearch(filter), onFinish);
  return (
    <div>
      <VersionBurndown {...props} animation={false} />
    </div>
  );
};
export default VersionBurndownComponent;
