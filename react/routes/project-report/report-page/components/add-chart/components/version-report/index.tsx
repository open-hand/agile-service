import React from 'react';
import { observer } from 'mobx-react-lite';
import VersionReport from '@/components/charts/version-report';
import VersionReportSearch from '@/components/charts/version-report/search';
import useVersionReport from '@/components/charts/version-report/useVersionReport';

const VersionReportComponent:React.FC = () => {
  const [props, searchProps] = useVersionReport();
  return (
    <div>
      <VersionReportSearch {...searchProps} />
      <VersionReport {...props} />
    </div>
  );
};

export default observer(VersionReportComponent);
