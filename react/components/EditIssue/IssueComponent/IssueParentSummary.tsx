import React from 'react';
import { observer } from 'mobx-react-lite';

interface Props {
  parentSummary: string
  disabled: boolean
  parentIssueId: string
  reloadIssue: Function
  resetIssue: Function
}

const IssueParentSummary: React.FC<Props> = ({
  parentSummary, disabled, parentIssueId, reloadIssue, resetIssue,
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

  return (
    <>
      {parentSummary ? (
        <span
          className="parent-summary"
          style={{
            display: 'inline-block', marginLeft: 15, fontSize: 16, lineHeight: '28px', fontWeight: 500, whiteSpace: 'nowrap',
          }}
        >
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
      ) : null}
    </>
  );
};

export default observer(IssueParentSummary);
