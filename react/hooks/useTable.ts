import { useUpdateEffect, usePersistFn, useMount } from 'ahooks';
import { useCallback, useState } from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import useControlledDefaultValue from './useControlledDefaultValue';

interface PaginatedParams {
  page: number
  size: number
  sort?: string
}
type TableRequest = ({ page, size, sort }: PaginatedParams) => Promise<any>
interface Options {
  autoQuery?: boolean
  defaultPage?: number
  defaultPageSize?: number
  defaultVisibleColumns?: string[]
}
export default function useTable(getData: TableRequest, options?: Options) {
  const {
    autoQuery = true, defaultPage, defaultPageSize, defaultVisibleColumns,
  } = options ?? {};
  const [pageSize, setPageSize] = useState(defaultPageSize ?? 10);
  const [current, setCurrent] = useState(defaultPage ?? 1);
  const [visibleColumns, setVisibleColumns] = useControlledDefaultValue(defaultVisibleColumns ?? []);
  const [total, setTotal] = useState(0);
  const [loading, setLoading] = useState(false);
  const [data, setData] = useState([]);
  const [checkValues, setCheckValues] = useState<string[]>([]);
  const [sort, setSort] = useState({ sortType: undefined, sortColumn: undefined });
  const handleCheckChange = usePersistFn((value) => {
    if (value) {
      checkValues.push(value);
    } else {
      checkValues.splice(checkValues.indexOf(value), 1);
    }
    setCheckValues([...checkValues]);
  });
  const handleCheckAllChange = usePersistFn((value) => {
    if (value) {
      // @ts-ignore
      setCheckValues(data.map((i) => i.issueId));
    } else {
      setCheckValues([]);
    }
  });
  const query = usePersistFn(async (newPage?: number) => {
    setLoading(true);
    const res = await getData({
      page: newPage ?? 1,
      size: pageSize,
      sort: sort.sortColumn && sort.sortType ? `${sort.sortColumn},${sort.sortType}` : undefined,
    });
    batchedUpdates(() => {
      setData(res.list);
      setTotal(res.total);
      setCurrent(res.number + 1);
      setLoading(false);
    });
  });

  useMount(() => {
    if (autoQuery) {
      query(current);
    }
  });
  useUpdateEffect(() => {
    query(current);
  }, [current, pageSize, sort]);
  const onPaginationChange = useCallback((page: number, size: number) => {
    setCurrent(page);
    setPageSize(size);
  }, [setCurrent]);
  const onShowSizeChange = useCallback((page: number, size: number) => {
    setPageSize(size);
  }, [setPageSize]);
  const handleSortColumn = usePersistFn((sortColumn, sortType) => {
    setSort({
      sortColumn,
      sortType,
    });
  });

  return {
    query,
    data,
    sortColumn: sort.sortColumn,
    sortType: sort.sortType,
    onSortColumn: handleSortColumn,
    loading,
    checkValues,
    handleCheckChange,
    handleCheckAllChange,
    visibleColumns,
    setVisibleColumns,
    pagination: {
      pageSize,
      current,
      total,
      onChange: onPaginationChange,
      onShowSizeChange,
    },
  };
}
