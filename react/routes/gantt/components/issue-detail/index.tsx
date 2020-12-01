// @ts-nocheck
import React, { useContext, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import EditIssue from '@/components/EditIssue';
import Context from '../../context';

const IssueDetail = () => {
  const { store } = useContext(Context);
  const { issueId } = store;
  const refresh = useCallback(() => [

  ], []);
  const handleResetIssue = useCallback((newIssueId) => {
    store.setIssueId(newIssueId);
  }, [store]);
  return (
    <EditIssue
      visible={issueId}
      disabled
      issueId={issueId}
      onCancel={() => {
        handleResetIssue(null);
      }}
      onDeleteIssue={() => {
        handleResetIssue(null);
        refresh();
      }}
      onUpdate={() => {
        refresh();
      }}
      resetIssue={(parentIssueId) => {
        handleResetIssue(parentIssueId);
      }}
    />
  );
};

export default observer(IssueDetail);
