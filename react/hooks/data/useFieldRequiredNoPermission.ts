import { pageConfigApi } from '@/api';
import { IField, IIssueType } from '@/common/types';
import { useMemo } from 'react';
import { useQuery, UseQueryOptions } from 'react-query';
import useProjectKey from './useProjectKey';

export interface IssueTypeConfig {
  issueTypeId?: string
  issueId?: string
  pageCode?: 'agile_issue_create' /** 创建的时候传 @default undefined */
}
/**
 * 获取必填字段但无权限查看
 * @param config
 * @param options
 * @returns
 */
export default function useFieldRequiredNoPermission(config?: IssueTypeConfig) {
  const { issueTypeId, pageCode, issueId } = config || {};

  const requiredNoPermissionList = useProjectKey({ key: ['requiredNoPermissionList', { issueTypeId }] });

  // eslint-disable-next-line no-nested-ternary
  return useQuery(requiredNoPermissionList, () => pageConfigApi.loadRequiredPermission({ issueTypeId: issueTypeId!, pageCode }, issueId) as Promise<IField[]>, { enabled: !!issueTypeId });
}
