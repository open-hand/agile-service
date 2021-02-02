import React from 'react';
import ServiceCodeQualityReport from '@/components/charts/service-code-quality';
import ServiceCodeQualitySearch from '@/components/charts/service-code-quality/search';
import { ServiceCodeQualitySearchVO } from '@/routes/project-report/report-page/store';
import useServiceCodeQualityReport from '@/components/charts/service-code-quality/useServiceCodeQualityReport';
import { transformServiceCodeQualitySearch } from '@/routes/project-report/report-page/components/add-chart/components/service-code-quality';

interface Props {
  filter: ServiceCodeQualitySearchVO
  onFinish?: Function
}
const ServiceCodeQualityComponent: React.FC<Props> = ({ filter, onFinish }) => {
  const [props, searchProps] = useServiceCodeQualityReport(transformServiceCodeQualitySearch(filter), onFinish);
  return (
    <div>
      <div style={{ display: 'none' }}>
        <ServiceCodeQualitySearch {...searchProps} />
      </div>
      <ServiceCodeQualityReport
        {...props}
        option={{
          animation: false,
        }}
      />
    </div>
  );
};
export default ServiceCodeQualityComponent;
