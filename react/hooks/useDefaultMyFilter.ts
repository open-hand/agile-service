import { find } from 'lodash';
import { useUnmount } from 'ahooks';
import useMyFilters from '@/hooks/data/useMyFilters';

export default function useDefaultMyFilter(projectId?: string) {
  const res = useMyFilters({ type: 'agile', projectId }, {
    staleTime: 0,
  });
  useUnmount(() => res.remove());
  return {
    ...res,
    data: find(res.data, { default: true }),
  };
}
