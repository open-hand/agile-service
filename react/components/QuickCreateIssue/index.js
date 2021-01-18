import React from 'react';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import useDefaultPriority from '@/hooks/useDefaultPriority';
import QuickCreateIssue from './QuickCreateIssue';

const QuickCreateIssueWithProvider = (props) => {
  const { data: issueTypes } = useProjectIssueTypes();
  const [defaultPriority] = useDefaultPriority();
  return (
    <QuickCreateIssue
      defaultPriority={defaultPriority}
      issueTypes={(issueTypes || []).filter(({ typeCode }) => !['issue_epic', 'feature', 'sub_task'].includes(typeCode))}
      {...props}
    />
  );
};
export default QuickCreateIssueWithProvider;
