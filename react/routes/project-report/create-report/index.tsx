import React, { useMemo, useEffect } from 'react';
import ReportPage from '../report-page';
import ProjectReportStore from '../report-page/store';

const CreateReport: React.FC = () => {
  const store = useMemo(() => new ProjectReportStore(), []);
  useEffect(() => {

  }, []);
  return <ReportPage store={store} />;
};
export default CreateReport;
