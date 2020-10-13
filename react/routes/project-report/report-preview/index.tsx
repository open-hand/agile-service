import React, {
  useMemo, useEffect, useCallback, useState,
} from 'react';
import { useParams } from 'react-router-dom';
import { projectReportApi } from '@/api';
import Loading from '@/components/Loading';
import ReportPage from '../report-page';
import ProjectReportStore from '../report-page/store';

const PreviewReport: React.FC = () => {
  const store = useMemo(() => new ProjectReportStore(), []);
  const [loading, setLoading] = useState(true);
  const { id } = useParams();
  const refresh = useCallback(async () => {
    setLoading(true);
    const res = await projectReportApi.getById(id);
    store.setReportData(res);
    setLoading(false);
  }, [id, store]);
  useEffect(() => {
    refresh();
  }, [refresh]);
  return loading ? <Loading loading /> : <ReportPage preview store={store} />;
};
export default PreviewReport;
