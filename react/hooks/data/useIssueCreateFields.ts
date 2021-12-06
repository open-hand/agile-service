import { useQueries, UseQueryResult } from 'react-query';
import { fieldApi, pageConfigApi } from '@/api';
import { IssueCreateFields } from '@/common/types';
import { ICascadeLinkage } from '@/routes/page-config/components/setting-linkage/Linkage';
import useProjectKey from './useProjectKey';

export interface IssueCreateFieldsConfig {
  issueTypeCode?: string
  issueTypeId: string
  projectId?: string
}
export default function useIssueCreateFields(config: IssueCreateFieldsConfig): [UseQueryResult<IssueCreateFields[]>, UseQueryResult<{ template?: string }>, UseQueryResult<ICascadeLinkage[]>, UseQueryResult<any[]>] {
  const { projectId, issueTypeId, issueTypeCode } = config;
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
    select: (data: any[]) => (issueTypeCode === 'feature' ? data.map((field) => {
      // 问题类型为特性时特殊处理某些字段
      switch (field.fieldCode) {
        case 'sprint':
          return { ...field, fieldType: 'multiple', fieldCode: 'subProjectSprint' };
        default:
          return field;
      }
    }) : data),
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
