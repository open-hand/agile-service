import { find } from 'lodash';
import { useUnmount } from 'ahooks';
import type {
  UseQueryOptions,
} from 'react-query';
import useMyFilters from '@/hooks/data/useMyFilters';
import { IPersonalFilter } from '@/components/quick-search';

export default function useDefaultMyFilter({ projectId, menuType }: { projectId?: string, menuType?: string } = {}, options?: UseQueryOptions<IPersonalFilter[]>) {
  const res = useMyFilters({ type: 'agile', projectId, menuType }, {
    staleTime: 0,
    ...options,
  });
  useUnmount(() => res.remove());
  return {
    ...res,
    data: find(res.data, { default: true }),
  };
}
