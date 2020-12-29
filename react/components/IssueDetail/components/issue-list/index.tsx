import React from 'react';
import { ISubIssue } from '@/common/types';
import IssueItem from './IssueItem';

interface IssueListProps {
  data: ISubIssue[]
  onClick?: (issue: ISubIssue) => void
}
const IssueList: React.FC<IssueListProps> = ({ data, onClick }) => (
  <div>
    {data.map((issue, index) => (
      <IssueItem i={index} showAssignee showPriority issue={issue} onClick={onClick} />
    ))}
  </div>
);

export default IssueList;
