import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { FlatSelect } from '@choerodon/components';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { issueApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import type { Issue } from '@/common/types';
import InlineIssueTag from '@/components/tag/inline-issue-tag';

export interface SelectParentIssueProps extends Partial<SelectProps> {
  issueType: 'sub_task' | 'bug'
  flat?: boolean
  projectId?: string
}

const SelectParentIssue: React.FC<SelectParentIssueProps> = forwardRef(({
  flat, projectId, issueType, ...otherProps
}, ref: React.Ref<Select>) => {
  const { multiple } = otherProps;
  const config = useMemo((): SelectConfig<Issue> => ({
    textField: 'summary',
    valueField: 'issueId',
    requestArgs: { issueType },
    request: ({ filter, page, requestArgs }) => (requestArgs?.issueType ? issueApi.project(projectId).loadParentIssues(page ?? 0, 20, requestArgs?.issueType, filter) : { list: [] }), // 故事、任务、缺陷（不能是子缺陷
    paging: true,
    optionRenderer: InlineIssueTag.createIssueTag({ multiple }),
    renderer: InlineIssueTag.createIssueTag({ rendererMode: true, multiple }),
  }), [issueType, multiple, projectId]);
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
export default SelectParentIssue;
