import React, {
  useMemo, useEffect, useCallback, useState,
} from 'react';
import { useParams } from 'react-router-dom';
import { projectReportApi } from '@/api';
import Loading from '@/components/Loading';
import ReportPage from '../report-page';
import ProjectReportStore from '../report-page/store';

const EditReport: React.FC = () => {
  const store = useMemo(() => new ProjectReportStore(), []);
  const [loading, setLoading] = useState(true);
  const { id } = useParams<any>();
  const refresh = useCallback(async () => {
    setLoading(true);
    const res = await projectReportApi.getById(id);
    store.setReportData(res);
    setLoading(false);
  }, [id, store]);
  useEffect(() => {
    refresh();
  }, [refresh]);
  return loading ? <Loading loading /> : <ReportPage refresh={refresh} edit store={store} />;
};
export default EditReport;
