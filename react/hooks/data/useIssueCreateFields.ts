import { fieldApi } from '@/api';
import { IssueCreateFields } from '@/common/types';
import { useQuery, UseQueryOptions } from 'react-query';
import useProjectKey from './useProjectKey';

export interface IssueCreateFieldsConfig {
  issueTypeId: string
  projectId?: string
}
export default function useIssueCreateFields(config: IssueCreateFieldsConfig, options?: UseQueryOptions<IssueCreateFields[]>) {
  const { projectId, issueTypeId } = config;
  const key = useProjectKey({ key: ['issue-create-fields', { issueTypeId }], projectId });
  return useQuery(key, () => fieldApi.getFields({
    schemeCode: 'agile_issue',
    issueTypeId,
    pageCode: 'agile_issue_create',
  }), {
    initialData: [] as IssueCreateFields[],
    keepPreviousData: true,
    enabled: !!issueTypeId,
    ...options,
  });
}
