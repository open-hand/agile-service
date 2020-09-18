import React from 'react';
import { observer } from 'mobx-react-lite';
import EpicReport from '@/components/charts/epic-report';
import EpicReportSearch from '@/components/charts/epic-report/search';
import useEpicReport from '@/components/charts/epic-report/useEpicReport';

const EpicReportComponent:React.FC = () => {
  const [props, searchProps] = useEpicReport();
  return (
    <div>
      <EpicReportSearch {...searchProps} />
      <EpicReport {...props} />
    </div>
  );
};

export default observer(EpicReportComponent);
