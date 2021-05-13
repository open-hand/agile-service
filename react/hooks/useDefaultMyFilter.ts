import useMyFilters from '@/hooks/data/useMyFilters';
import { find } from 'lodash';

export default function useDefaultMyFilter() {
  const res = useMyFilters({ type: 'agile' });
  return {
    ...res,
    data: find(res.data, { default: true }),
  };
}
