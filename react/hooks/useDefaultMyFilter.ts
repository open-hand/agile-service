import useMyFilters from '@/hooks/data/useMyFilters';
import { find } from 'lodash';
import { useUnmount } from 'ahooks';

export default function useDefaultMyFilter() {
  const res = useMyFilters({ type: 'agile' }, {
    staleTime: 0,
  });
  useUnmount(() => res.remove());
  return {
    ...res,
    data: find(res.data, { default: true }),
  };
}
