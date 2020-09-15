import { useState, useEffect, useCallback } from 'react';
import { personalFilterApi } from '@/api';

export default function useMyFilters() {
  const [myFilters, setMyFilters] = useState([]);
  const [loading, setLoading] = useState(false);
  const refresh = useCallback(async () => {
    setLoading(true);
    const data = await personalFilterApi.loadAll();
    setMyFilters(data);
    setLoading(false);
  }, []);
  useEffect(() => {
    refresh();
  }, [refresh]);
  return {
    data: myFilters,
    loading,
    refresh,
  };
}
