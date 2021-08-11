import React, {
  useMemo, useEffect, useCallback, useState,
} from 'react';
import { useParams } from 'react-router-dom';
import { projectReportApi } from '@/api';
import Loading from '@/components/Loading';
import useQueryString from '@/hooks/useQueryString';
import ReportPage from '../report-page';
import ProjectReportStore from '../report-page/store';

const PreviewReport: React.FC = () => {
  const store = useMemo(() => new ProjectReportStore(), []);
  const [loading, setLoading] = useState(true);
  const { id } = useParams();
  const params = useQueryString();
  const refresh = useCallback(async () => {
    setLoading(true);
    const data = window.opener && params.dataKey ? window.opener[params.dataKey] : null;
    const res = data || await projectReportApi.getById(id);
    store.setReportData(res);
    setLoading(false);
  }, [id, store]);
  useEffect(() => {
    refresh();
  }, [refresh]);
  return loading ? <Loading loading /> : <ReportPage preview store={store} />;
};
export default PreviewReport;
