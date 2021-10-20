import {
  useQuery, UseQueryOptions, useMutation, useQueryClient,
} from 'react-query';
import { cacheColumnApi, IListLayout } from '@/api';
import useProjectKey from './useProjectKey';

export interface TableColumnConfig {
  type: string
  projectId?: string
}
export function useTableColumnsKey(type: string, projectId?: string) {
  return useProjectKey({ key: ['TableColumns', type], projectId });
}
export default function useTableColumns(config: TableColumnConfig, options?: UseQueryOptions<IListLayout>) {
  const key = useTableColumnsKey(config.type, config.projectId);
  return useQuery(key, () => cacheColumnApi.project(config.projectId).getDefault(config.type), {
    ...options,
  });
}
export const useUpdateColumnMutation = (type: string, projectId?: string) => {
  const queryClient = useQueryClient();
  const key = useTableColumnsKey(type, projectId);
  return useMutation(
    (listLayout: IListLayout) => cacheColumnApi.project(projectId).update(listLayout),
    {
      onSuccess: (data) => {
        // 用返回数据更新
        queryClient.setQueryData(key, data);
      },
      onSettled: () => {
        queryClient.invalidateQueries(key);
      },
    },
  );
};
