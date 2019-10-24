import React from 'react';
import { observer } from 'mobx-react-lite';
import { withRouter } from 'react-router-dom';
import { programIssueLink, issueLink } from '../../../common/utils';


const IssueNumber = ({
  parentIssueId, resetIssue, reloadIssue, typeCode, parentIssueNum, issue, type, history, disabled,
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
    history.push(typeCode === 'feature' ? programIssueLink(issueId, issueNum) : issueLink(issueId, typeCode, issueNum));
    return false;
  };


  const { issueNum } = issue;
  return (
    <div style={{ fontSize: 16, lineHeight: '28px', fontWeight: 500 }}>
      {
        parentIssueNum ? (
          <span>
            <span
              role="none"
              className="primary"
              style={{ cursor: disabled ? 'auto' : 'pointer' }}
              onClick={handleClickParent}
            >
              {parentIssueNum}
            </span>
            <span style={{ paddingLeft: 10, paddingRight: 10 }}>/</span>
          </span>
        ) : null
      }
      {
        (['sub_task', 'bug'].includes(typeCode) && parentIssueNum) ? (
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
    </div>
  );
};

export default withRouter(IssueNumber);
