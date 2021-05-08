import React from 'react';
import { usePersistFn, useUnmount } from 'ahooks';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import Loading from '@/components/Loading';
import useTableColumns from '@/hooks/data/useTableColumns';

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
  type: string
  children: (props: TableCacheRenderProps) => React.ReactElement<any, any> | null
}
const TableCache: React.FC<TableCacheProps> = ({
  type, children,
}) => {
  const cached = localPageCacheStore.getItem(type);
  const { isLoading, data } = useTableColumns({ type });
  const updateCache = usePersistFn(({ pagination, visibleColumns }) => {
    localPageCacheStore.setItem(type, {
      pagination,
      visibleColumns,
    });
  });
  if (isLoading) {
    return <Loading loading />;
  }
  return children({
    cached: {
      visibleColumns: data?.listLayoutColumnRelVOS.map((c) => c.columnCode),
      ...cached,
    },
    updateCache,
  });
};

export default TableCache;
