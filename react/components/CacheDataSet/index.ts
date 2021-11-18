import { DataSet } from 'choerodon-ui/pro';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';

interface DataSetCache {
  pageSize: number,
  currentPage: number,
  queryParameter: Object,
}
export interface CacheDataSetProps extends DataSetProps {
  cacheKey: string
}
export default function CacheDataSet({ cacheKey, ...props }: CacheDataSetProps) {
  const {
    pageSize = 10,
    currentPage = 1,
    queryParameter,
  } = (localPageCacheStore.getItem(cacheKey) || {}) as DataSetCache;
  const dataSet = new DataSet({
    ...props,
    pageSize: pageSize || 10,
    autoQuery: false,
    queryParameter,
  });
  dataSet.query(currentPage);
  dataSet.addEventListener('load', () => {
    localPageCacheStore.setItem(cacheKey, {
      pageSize: dataSet.pageSize,
      currentPage: dataSet.currentPage,
      queryParameter: dataSet.queryParameter,
    });
  });
  return dataSet;
}
