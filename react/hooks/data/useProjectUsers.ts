import { userApi } from '@/api';
import { User } from '@/common/types';
import {
  useQuery, UseQueryOptions, UseInfiniteQueryOptions, useInfiniteQuery,
} from 'react-query';
import useProjectKey from './useProjectKey';

export interface ProjectUsersConfig {
  param?: string
  page?: number
  size?: number
  projectId?: string
}
export default function useProjectUsers(config?: ProjectUsersConfig, options?: UseInfiniteQueryOptions<{
  list: User[]
  hasNextPage: boolean
  number: number
}>) {
  const {
    param, page = 1, size = 10, projectId,
  } = config || {};
  const key = useProjectKey({ key: ['users', { page, param }], projectId: config?.projectId });

  const props = useInfiniteQuery(key, ({ pageParam = 1 }) => userApi.getAllInProject(param, pageParam, undefined, size, projectId), {
    // 目前没有查询上一页的场景
    getPreviousPageParam: (firstPage) => false,
    getNextPageParam: (lastPage) => (lastPage.hasNextPage ? lastPage.number + 2 : false),
    staleTime: 100000,
    ...options,
  });
  return {
    ...props,
    data: props.data?.pages.reduce((total, pageData) => [...total, ...pageData.list], []) ?? [],
  };
}
