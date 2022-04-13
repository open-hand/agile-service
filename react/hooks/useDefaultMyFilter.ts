import { find } from 'lodash';
import { useUnmount } from 'ahooks';
import type {
  UseQueryOptions,
} from 'react-query';
import useMyFilters from '@/hooks/data/useMyFilters';
import { IPersonalFilter } from '@/components/quick-search';

interface Props {
  projectId?: string,
  menuType?: string,
  filterTypeCode?: 'agile_issue' | 'risk_issue',
  type?: string,
}

export default function useDefaultMyFilter({
  projectId, menuType, filterTypeCode, type,
}: Props = {}, options?: UseQueryOptions<IPersonalFilter[]>) {
  const res = useMyFilters({
    type: type || 'agile', projectId, menuType, filterTypeCode,
  }, {
    staleTime: 0,
    ...options,
  });
  useUnmount(() => res.remove());
  return {
    ...res,
    data: find(res.data, { default: true }),
  };
}
