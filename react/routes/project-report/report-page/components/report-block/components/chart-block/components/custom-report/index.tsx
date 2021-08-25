import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { CustomReportSearchVO } from '@/routes/project-report/report-page/store';
import Chart from '@/routes/ReportHost/custom-report/components/Chart';
import useReport from '@/routes/ReportHost/custom-report/components/Chart/useReport';
import { transformCustomReportSearch } from '@/routes/project-report/report-page/components/add-chart/components/custom-report';

interface Props {
  filter: CustomReportSearchVO,
  onFinish?: Function
}
const CustomReportComponents: React.FC<Props> = ({ filter, onFinish }) => {
  const config = useMemo(() => transformCustomReportSearch(filter), [filter]);
  // @ts-ignore
  const [, props] = useReport(config, 18, onFinish);
  return (
    <div>
      <Chart {...props} />
    </div>
  );
};
export default observer(CustomReportComponents);
