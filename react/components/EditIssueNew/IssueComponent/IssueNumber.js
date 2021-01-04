import React from 'react';
import { withRouter } from 'react-router-dom';
import { LINK_URL_TO } from '@/constants/LINK_URL';
import IssueParentTip from './IssueParentTip';
import IssueSwitch from './IssueSwitch';

const IssueNumber = ({
  reloadIssue, typeCode, parentSummary, issue, disabled,
}) => {
  const handleClickIssueNum = () => {
    if (disabled) {
      return false;
    }
    const { issueId, issueNum } = issue;
    LINK_URL_TO.issueLinkTo(issueId, issueNum);
    return false;
  };

  const { issueNum } = issue;
  return (
    <div style={{
      fontSize: 16, lineHeight: '28px', fontWeight: 500, whiteSpace: 'nowrap',
    }}
    >
      {
        ((['sub_task', 'bug'].includes(typeCode) && parentSummary) || typeCode === 'feature') ? (
          <span>
            {issueNum}
          </span>
        ) : (
          <a
            role="none"
            onClick={handleClickIssueNum}
            style={{ cursor: disabled ? 'auto' : 'pointer' }}
          >
            {issueNum}
          </a>
        )
      }
      <IssueSwitch issue={issue} reloadIssue={reloadIssue} />
    </div>
  );
};

export default withRouter(IssueNumber);
