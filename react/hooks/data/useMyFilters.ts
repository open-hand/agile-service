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
  projectId?: string
  menuType?: string
}
export function useMyFiltersKey(type: string, projectId?: string) {
  return useProjectKey({ key: ['myFilters', type], projectId });
}
export default function useMyFilters(config: MyFiltersConfig, options?: UseQueryOptions<IPersonalFilter[]>) {
  const key = useMyFiltersKey(config.type, config.projectId);
  return useQuery(key, () => personalFilterApi.menu(config.menuType as any).project(config.projectId).loadAll(), {
    ...options,
  });
}
export const useUpdateMyFilterMutation = (type: string, projectId?: string) => {
  const queryClient = useQueryClient();
  const key = useMyFiltersKey(type, projectId);
  return useMutation(
    ({ filterId, data }: { filterId: string, data: UPersonalFilter }) => personalFilterApi.project(projectId).update(filterId, data),
    {
      onSettled: () => {
        queryClient.invalidateQueries(key);
      },
    },
  );
};
