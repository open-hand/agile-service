import React from 'react';
import VersionReport from '@/components/charts/version-report';
import useVersionReport from '@/components/charts/version-report/useVersionReport';
import { VersionReportSearchVO } from '@/routes/project-report/report-page/store';
import { transformVersionReportSearch } from '@/routes/project-report/report-page/components/add-chart/components/version-report';

interface Props {
  filter: VersionReportSearchVO
  onFinish?: Function
}
const VersionReportComponent: React.FC<Props> = ({ filter, onFinish }) => {
  const [props] = useVersionReport(transformVersionReportSearch(filter), onFinish);
  return (
    <div>
      <VersionReport {...props} animation={false} />
    </div>
  );
};
export default VersionReportComponent;
