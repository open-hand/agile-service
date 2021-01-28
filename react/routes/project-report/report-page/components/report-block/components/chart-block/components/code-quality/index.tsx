import React from 'react';
import CodeQualityReport from '@/components/charts/code-quality';
import { VersionReportSearchVO } from '@/routes/project-report/report-page/store';
import useCodeQualityReport from '@/components/charts/code-quality/useCodeQualityReport';
import { transformCodeQualitySearch } from '@/routes/project-report/report-page/components/add-chart/components/code-quality';

interface Props {
  filter: VersionReportSearchVO
  onFinish?: Function
}
const CodeQualityComponent: React.FC<Props> = ({ filter, onFinish }) => {
  const [props] = useCodeQualityReport(transformCodeQualitySearch(filter), onFinish);
  return (
    <div>
      <CodeQualityReport
        {...props}
        option={{
          animation: false,
        }}
      />
    </div>
  );
};
export default CodeQualityComponent;
