import { fieldApi } from '@/api';
import { IField } from '@/common/types';
import { useQuery, UseQueryOptions } from 'react-query';
import useProjectKey from './useProjectKey';

export interface IssueTableFieldsConfig {

}
export default function useIssueTableFields(config?: IssueTableFieldsConfig, options?: UseQueryOptions<IField[]>) {
  const key = useProjectKey({ key: ['IssueTableFields'] });
  return useQuery(key, () => fieldApi.getTableFields(), {
    initialData: [] as IField[],
    ...options,
  });
}
