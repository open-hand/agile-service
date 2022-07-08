import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { FlatSelect } from '@choerodon/components';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { workingHoursApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import type { Issue } from '@/common/types';
import InlineIssueTag from '@/components/tag/inline-issue-tag';

interface Props extends Partial<SelectProps> {
  flat?: boolean
  projectId?: string
  issueId?: string,
}

const SelectIssuesDailyLog: React.FC<Props> = forwardRef(({
  flat, projectId, issueId, ...otherProps
}, ref: React.Ref<Select>) => {
  const { multiple } = otherProps;
  const config = useMemo((): SelectConfig<Issue> => ({
    textField: 'summary',
    valueField: 'issueId',
    request: ({ filter, page }) => workingHoursApi.project(projectId).loadIssuesDailyLog(page, 20, filter ? undefined : issueId, filter), // 故事、任务、缺陷（不能是子缺陷
    paging: true,
    optionRenderer: InlineIssueTag.createIssueTag({ multiple }),
    renderer: InlineIssueTag.createIssueTag({ multiple, rendererMode: true }),
  }), [issueId, multiple, projectId]);
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
export default SelectIssuesDailyLog;
