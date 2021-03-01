import { useState, useEffect, useCallback } from 'react';
import { quickFilterApi } from '@/api';
import { IQuickFilter } from '@/components/quick-search';

export default function useQuickFilters({ projectId }: { projectId?: string }) {
  const [data, setData] = useState<IQuickFilter[]>([]);
  const [loading, setLoading] = useState(false);
  const refresh = useCallback(async () => {
    setLoading(true);
    const res = await quickFilterApi.project(projectId).loadAll();
    setData(res);
    setLoading(false);
  }, [projectId]);
  useEffect(() => {
    refresh();
  }, [refresh]);
  return {
    data,
    loading,
    refresh,
  };
}
