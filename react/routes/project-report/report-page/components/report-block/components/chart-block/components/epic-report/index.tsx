import React from 'react';
import EpicReport from '@/components/charts/epic-report';
import useEpicReport from '@/components/charts/epic-report/useEpicReport';
import { EpicReportSearchVO } from '@/routes/project-report/report-page/store';
import { transformEpicReportSearch } from '@/routes/project-report/report-page/components/add-chart/components/epic-report';

interface Props {
  filter: EpicReportSearchVO
  onFinish?: Function
}
const EpicReportComponent: React.FC<Props> = ({ filter, onFinish }) => {
  const [props] = useEpicReport(transformEpicReportSearch(filter), onFinish);
  return (
    <div>
      <EpicReport {...props} animation={false} />
    </div>
  );
};
export default EpicReportComponent;
