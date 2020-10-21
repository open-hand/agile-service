import React from 'react';
import { withRouter } from 'react-router-dom';
import { LINK_URL_TO } from '@/constants/LINK_URL';
import IssueParentTip from './IssueParentTip';
import IssueSwitch from './IssueSwitch';

const IssueNumber = ({
  parentIssueId, resetIssue, reloadIssue, typeCode, parentSummary, parentDescription, issue, type, disabled,
}) => {
  const handleClickParent = () => {
    if (disabled) {
      return false;
    }
    if (reloadIssue) {
      reloadIssue(parentIssueId);
    }
    if (resetIssue) {
      resetIssue(parentIssueId);
    }
    return false;
  };

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
        parentSummary ? (
          <span className="parent-summary" style={{ display: 'inline-block', width: '90%', maxWidth: 'max-content' }}>
            <div className="parent-summary-tip">
              <IssueParentTip
                parentSummary={parentSummary}
                parentDescription={parentDescription}
              />
            </div>
            <span
              role="none"
              className="primary parent-summary-hidden"
              style={{ cursor: disabled ? 'auto' : 'pointer' }}
              onClick={handleClickParent}
            >
              {parentSummary}
            </span>
            <span style={{ paddingLeft: 10, paddingRight: 10 }}>/</span>
          </span>
        ) : null
      }
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
