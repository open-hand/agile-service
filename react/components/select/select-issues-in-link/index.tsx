import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { issueApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import type { Issue } from '@/common/types';
import { FlatSelect } from '@choerodon/components';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

interface Props extends Partial<SelectProps> {
  flat?: boolean
  projectId?: string
  issueId?: string,
  excludeIssueIds?: string[],
}

const SelectIssuesInLink: React.FC<Props> = forwardRef(({
  flat, projectId, issueId, excludeIssueIds, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<Issue> => ({
    textField: 'summary',
    valueField: 'issueId',
    tooltip: true,
    request: ({ filter, page }) => issueApi.project(projectId).loadIssuesInLink(page, 20, issueId, filter, excludeIssueIds), // 故事、任务、缺陷（不能是子缺陷
    paging: true,
  }), [excludeIssueIds, issueId, projectId]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      clearButton
      {...props}
      {...otherProps}
    />
  );
});
export default SelectIssuesInLink;
