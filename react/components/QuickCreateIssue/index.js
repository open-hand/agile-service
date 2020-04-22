import React, { useContext } from 'react';
import useIssueTypes from '@/hooks/useIssueTypes';
import useDefaultPriority from '@/hooks/useDefaultPriority';
import QuickCreateIssue from './QuickCreateIssue';

const QuickCreateIssueWithProvider = (props) => {
  const [issueTypes] = useIssueTypes();
  const [defaultPriority] = useDefaultPriority();
  return (
    <QuickCreateIssue
      defaultPriority={defaultPriority}
      issueTypes={issueTypes.filter(({ typeCode }) => !['issue_epic', 'feature', 'sub_task'].includes(typeCode))}
      {...props}
    />
  );
};
export default QuickCreateIssueWithProvider;
