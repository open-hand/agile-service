import {
  useQuery, UseQueryOptions, useMutation, useQueryClient,
} from 'react-query';
import {
  personalFilterApi, UPersonalFilter,
} from '@/api';
import { IPersonalFilter } from '@/components/quick-search';
import useProjectKey from './useProjectKey';

export interface MyFiltersConfig {
  type: string
}
export function useMyFiltersKey(type: string) {
  return useProjectKey({ key: ['myFilters', type] });
}
export default function useMyFilters(config: MyFiltersConfig, options?: UseQueryOptions<IPersonalFilter[]>) {
  const key = useMyFiltersKey(config.type);
  return useQuery(key, () => personalFilterApi.loadAll(), {
    ...options,
  });
}
export const useUpdateMyFilterMutation = (type: string) => {
  const queryClient = useQueryClient();
  const key = useMyFiltersKey(type);
  return useMutation(
    ({ filterId, data }: { filterId: string, data: UPersonalFilter }) => personalFilterApi.update(filterId, data),
    {
      onSettled: () => {
        queryClient.invalidateQueries(key);
      },
    },
  );
};
