import takeLast from '@/utils/takeLast';
import {
  useUpdateEffect, usePersistFn, useMount, useLockFn,
} from 'ahooks';
import {
  intersection, get, uniq, find,
} from 'lodash';
import { useCallback, useMemo, useState } from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';

export interface TreeShape {
  children?: TreeShape[]
  [key: string]: any
}
interface FlatShape {
  parentId: string
  [key: string]: any
}

function transverseTreeData(data: FlatShape[], rowKey: string): TreeShape[] {
  const res = [];
  const map = new Map<string, TreeShape>(data.map((item) => ([get(item, rowKey), { ...item }])));
  for (const [, item] of map) {
    if (item.parentId) {
      const parent = map.get(item.parentId);
      if (parent) {
        if (!parent.children) {
          parent.children = [];
        }
        parent.children.push(item);
      }
    } else {
      res.push(item);
    }
  }
  return res;
}

interface PaginatedParams {
  page: number
  size: number
  sort?: string
  isTree: boolean
}
type TableRequest = ({
  page, size, sort, isTree,
}: PaginatedParams) => Promise<any>
interface Options {
  rowKey: string
  autoQuery?: boolean
  defaultPage?: number
  defaultPageSize?: number
  defaultVisibleColumns?: string[]
  defaultChecked?: string[]
  isTree?: boolean
  checkBefore?: () => void
}
export default function useTable(getData: TableRequest, options: Options) {
  const {
    autoQuery = true, defaultPage, defaultPageSize, defaultChecked, defaultVisibleColumns, isTree = true, rowKey, checkBefore,
  } = options ?? {};
  const [pageSize, setPageSize] = useState(defaultPageSize ?? 10);
  const [current, setCurrent] = useState(defaultPage ?? 1);
  const [total, setTotal] = useState(0);
  const [loading, setLoading] = useState(false);
  const [data, setData] = useState([]);
  const [checkValues, setCheckValues] = useState<string[]>(defaultChecked ?? []);
  const [expandedRowKeys, setExpandedRowKeys] = useState<string[]>([]);
  const [sort, setSort] = useState({ sortType: undefined, sortColumn: undefined });
  const handleCheckChange = usePersistFn((value, key) => {
    if (value) {
      if (checkBefore) {
        checkBefore();
      }
      checkValues.push(key);
    } else {
      checkValues.splice(checkValues.indexOf(key), 1);
    }
    setCheckValues([...checkValues]);
  });
  const handleCheckAllChange = usePersistFn((value) => {
    if (value) {
      if (checkBefore) {
        checkBefore();
      }
      setCheckValues(uniq([...checkValues, ...data.map((i) => get(i, rowKey))]));
    } else {
      setCheckValues(checkValues.filter((key) => !find(data, { [rowKey]: key })));
    }
  });
  const request = useMemo(() => takeLast(getData), [getData]);
  const query = usePersistFn(async (newPage?: number) => {
    setLoading(true);
    try {
      const res = await request({
        page: newPage ?? 1,
        size: pageSize,
        sort: sort.sortColumn && sort.sortType ? `${sort.sortColumn},${sort.sortType}` : undefined,
        isTree,
      });
      batchedUpdates(() => {
        setData(res.list);
        setTotal(res.total);
        setCurrent(res.number + 1);
        setLoading(false);
      });
    } catch (error) {
      //
    }
  });

  useMount(() => {
    if (autoQuery) {
      query(current);
    }
  });
  useUpdateEffect(() => {
    query(current);
  }, [current, pageSize, sort, isTree]);
  const onPaginationChange = useCallback((page: number, size: number) => {
    if (size !== pageSize) {
      setCurrent(defaultPage ?? 1);
    } else {
      setCurrent(page);
    }
    setPageSize(size);
  }, [setCurrent, pageSize]);
  const onShowSizeChange = useCallback((page: number, size: number) => {
    setPageSize(size);
  }, [setPageSize]);
  const handleSortColumn = usePersistFn((sortColumn, sortType) => {
    setSort({
      sortColumn,
      sortType,
    });
  });
  const onExpandChange = usePersistFn((expanded: boolean, rowData: any) => {
    if (expanded) {
      setExpandedRowKeys((e) => ([...e, get(rowData, rowKey)]));
    } else {
      setExpandedRowKeys((e) => (e.filter((k) => k !== get(rowData, rowKey))));
    }
  });
  const treeData = useMemo(() => (isTree ? transverseTreeData(data, rowKey) : data), [isTree, data, rowKey]);
  const expandAbleKeys = useMemo(() => treeData.filter((d) => d.children && d.children.length > 0).map((d) => get(d, rowKey)), [rowKey, treeData]);
  const expandAll = usePersistFn((value: boolean) => {
    if (value) {
      setExpandedRowKeys(expandAbleKeys);
    } else {
      setExpandedRowKeys([]);
    }
  });
  const isExpandAll = useMemo(() => intersection(expandAbleKeys, expandedRowKeys).length === expandAbleKeys.length, [expandAbleKeys, expandedRowKeys]);

  return {
    query,
    rowKey,
    data: treeData,
    flattenData: data,
    isTree,
    sortColumn: sort.sortColumn,
    sortType: sort.sortType,
    onSortColumn: handleSortColumn,
    loading,
    checkValues,
    handleCheckChange,
    handleCheckAllChange,
    setCheckValues,
    expandAbleKeys,
    expandAll,
    isExpandAll,
    expandedRowKeys,
    onExpandChange,
    pagination: {
      pageSize,
      current,
      total,
      onChange: onPaginationChange,
      onShowSizeChange,
    },
  };
}
