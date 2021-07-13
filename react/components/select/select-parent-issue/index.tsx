import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { issueApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import type { Issue } from '@/common/types';
import { FlatSelect } from '@choerodon/components';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

interface Props extends Partial<SelectProps> {
  issueType: 'sub_task' | 'bug'
  flat?: boolean
  projectId?: string
}

const SelectParentIssue: React.FC<Props> = forwardRef(({
  flat, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<Issue> => ({
    textField: 'summary',
    valueField: 'issueId',
    tooltip: true,
    request: ({ filter, page }) => issueApi.project(projectId).loadParentIssues(page ?? 0, 20, 'sub_task', filter), // 故事、任务、缺陷（不能是子缺陷

    paging: true,
  }), [projectId]);
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
