import useProjectKey from '@/hooks/data/useProjectKey';
import React from 'react';
import { useQuery } from 'react-query';
import { usePersistFn, useUnmount } from 'ahooks';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { cacheColumnApi } from '@/api';
import Loading from '@/components/Loading';

interface Cache {
  pagination: {
    current: number,
    pageSize: number
  }
  visibleColumns: string[]
}
interface TableCacheRenderProps {
  cached: Cache
  updateCache: (cache: Cache) => void
}
export interface TableCacheProps {
  cacheKey: string
  children: (props: TableCacheRenderProps) => React.ReactElement<any, any> | null
}
const TableCache: React.FC<TableCacheProps> = ({
  cacheKey, children,
}) => {
  const key = useProjectKey({ key: [cacheKey] });
  const cached = localPageCacheStore.getItem(cacheKey);
  const { isLoading, data } = useQuery(key, () => cacheColumnApi.getDefault(cacheKey));
  const updateCache = usePersistFn(({ pagination, visibleColumns }) => {
    localPageCacheStore.setItem(cacheKey, {
      pagination,
      visibleColumns,
    });
  });
  if (isLoading) {
    return <Loading loading />;
  }
  return children({
    cached,
    updateCache,
  });
};

export default TableCache;
