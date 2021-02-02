import React from 'react';
import CodeQualityVaryReport from '@/components/charts/code-quality-vary';
import { CodeQualityVarySearchVO } from '@/routes/project-report/report-page/store';
import useCodeQualityVaryReport from '@/components/charts/code-quality-vary/useCodeQualityVaryReport';
import { transformCodeQualityVarySearch } from '@/routes/project-report/report-page/components/add-chart/components/code-quality-vary';

interface Props {
  filter: CodeQualityVarySearchVO
  onFinish?: Function
}
const CodeQualityVaryComponent: React.FC<Props> = ({ filter, onFinish }) => {
  const [props] = useCodeQualityVaryReport(transformCodeQualityVarySearch(filter), onFinish);
  return (
    <div>
      <CodeQualityVaryReport
        {...props}
        option={{
          animation: false,
        }}
      />
    </div>
  );
};
export default CodeQualityVaryComponent;
