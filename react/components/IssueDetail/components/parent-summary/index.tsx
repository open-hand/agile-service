import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { useDetailContext } from '../../context';
import styles from './index.less';

const ParentSummary = () => {
  const { store } = useDetailContext();
  const {
    parentIssueSummary, parentIssueId, parentRelateSummary, relateIssueId,
  } = store.issue;
  const parentId = parentIssueId || relateIssueId;
  const parentSummary = parentIssueSummary || parentRelateSummary;
  const handleClickParent = useCallback(() => {
    if (parentId) {
      store.select(parentId);
      store.refresh();
    }
  }, [parentId, store]);

  return (
    <>
      {parentSummary ? (
        <span
          className={styles.parent_summary}
        >
          <span
            role="none"
            className={styles.parent_summary_hidden}
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

export default observer(ParentSummary);
