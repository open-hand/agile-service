import { useState, useEffect, useCallback } from 'react';
import { quickFilterApi } from '@/api';
import { IQuickFilter } from '@/components/quick-search';

export default function useQuickFilters() {
  const [data, setData] = useState<IQuickFilter[]>([]);
  const [loading, setLoading] = useState(false);
  const refresh = useCallback(async () => {
    setLoading(true);
    const res = await quickFilterApi.loadAll();
    setData(res);
    setLoading(false);
  }, []);
  useEffect(() => {
    refresh();
  }, [refresh]);
  return {
    data,
    loading,
    refresh,
  };
}
