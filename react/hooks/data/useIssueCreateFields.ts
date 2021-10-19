import { useQueries, UseQueryResult } from 'react-query';
import { fieldApi, pageConfigApi } from '@/api';
import { IssueCreateFields } from '@/common/types';
import { ICascadeLinkage } from '@/routes/page-config/components/setting-linkage/Linkage';
import useProjectKey from './useProjectKey';

export interface IssueCreateFieldsConfig {
  issueTypeId: string
  projectId?: string
}
export default function useIssueCreateFields(config: IssueCreateFieldsConfig): [UseQueryResult<IssueCreateFields[]>, UseQueryResult<{ template?: string }>, UseQueryResult<ICascadeLinkage[]>, UseQueryResult<any[]>] {
  const { projectId, issueTypeId } = config;
  const fieldsKey = useProjectKey({ key: ['issue-create-fields', { issueTypeId }], projectId });
  const templateKey = useProjectKey({ key: ['issue-create description-template', { issueTypeId }], projectId });
  const pageCascadeRuleList = useProjectKey({ key: ['issue-create pageCascadeRuleList', { issueTypeId }], projectId });

  // @ts-ignore
  return useQueries([{
    queryKey: fieldsKey,
    queryFn: () => fieldApi.getFields({
      schemeCode: 'agile_issue',
      issueTypeId,
      pageCode: 'agile_issue_create',
    }, projectId),
    initialData: [] as IssueCreateFields[],
    keepPreviousData: true,
    enabled: !!issueTypeId,
  }, {
    queryKey: templateKey,
    queryFn: () => pageConfigApi.loadTemplateByType(issueTypeId, projectId),
    keepPreviousData: true,
    enabled: !!issueTypeId,
  }, {
    queryKey: pageCascadeRuleList,
    queryFn: () => pageConfigApi.project(projectId).getCascadeRuleList(issueTypeId),
    keepPreviousData: true, // ?
    enabled: !!issueTypeId,
  },
  ]);
}
