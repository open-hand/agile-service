import React from 'react';
import VersionReport from '@/components/charts/version-report';
import useVersionReport from '@/components/charts/version-report/useVersionReport';
import { VersionReportSearchVO } from '@/routes/project-report/report-page/store';
import { transformVersionReportSearch } from '@/routes/project-report/report-page/components/add-chart/components/version-report';

interface Props {
  filter: VersionReportSearchVO
}
const VersionReportComponent: React.FC<Props> = ({ filter }) => {
  const [props] = useVersionReport(transformVersionReportSearch(filter));
  return (
    <div>
      <VersionReport {...props} />
    </div>
  );
};
export default VersionReportComponent;
