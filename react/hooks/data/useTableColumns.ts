import {
  useQuery, UseQueryOptions, useMutation, useQueryClient,
} from 'react-query';
import { cacheColumnApi, IListLayout } from '@/api';
import useProjectKey from './useProjectKey';

export interface TableColumnConfig {
  type: string
}
export function useTableColumnsKey(type: string) {
  return useProjectKey({ key: ['TableColumns', type] });
}
export default function useTableColumns(config: TableColumnConfig, options?: UseQueryOptions<IListLayout>) {
  const key = useTableColumnsKey(config.type);
  return useQuery(key, () => cacheColumnApi.getDefault(config.type), {
    ...options,
  });
}
export const useUpdateColumnMutation = (type: string) => {
  const queryClient = useQueryClient();
  const key = useTableColumnsKey(type);
  return useMutation(
    (listLayout: IListLayout) => cacheColumnApi.update(listLayout),
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
