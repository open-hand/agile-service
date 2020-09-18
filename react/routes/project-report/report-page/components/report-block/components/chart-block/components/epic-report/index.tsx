import React from 'react';
import EpicReport from '@/components/charts/epic-report';
import useEpicReport from '@/components/charts/epic-report/useEpicReport';
import { EpicReportSearchVO } from '@/routes/project-report/report-page/store';
import { transformEpicReportSearch } from '@/routes/project-report/report-page/components/add-chart/components/epic-report';

interface Props {
  filter: EpicReportSearchVO
}
const EpicReportComponent: React.FC<Props> = ({ filter }) => {
  const [props] = useEpicReport(transformEpicReportSearch(filter));
  return (
    <div>
      <EpicReport {...props} />
    </div>
  );
};
export default EpicReportComponent;
