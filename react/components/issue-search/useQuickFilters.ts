import { useState, useEffect, useCallback } from 'react';
import { UseQueryOptions } from 'react-query';
import { quickFilterApi } from '@/api';

import { IQuickFilter } from '@/components/quick-search';
/**
 * 获取快速筛选数据hook
 * @param param0
 * @param options
 * @returns
 */
export default function useQuickFilters({ projectId }: { projectId?: string }, options: Pick<UseQueryOptions<any>, 'enabled'> = { enabled: true }) {
  const [data, setData] = useState<IQuickFilter[]>([]);
  const [loading, setLoading] = useState(false);

  const refresh = useCallback(async () => {
    setLoading(true);
    const res = await quickFilterApi.project(projectId).loadAll();
    setData(res);
    setLoading(false);
  }, [projectId]);
  useEffect(() => {
    options?.enabled && refresh();
  }, [options?.enabled, refresh]);
  return {
    data,
    loading,
    refresh,
  };
}
