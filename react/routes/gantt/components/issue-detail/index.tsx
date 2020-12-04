import React, { useContext, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import EditIssue from '@/components/EditIssue';
import Context from '../../context';

interface Props {
  refresh: () => void
}
const IssueDetail:React.FC<Props> = ({ refresh }) => {
  const { store } = useContext(Context);
  const { issueId } = store;
  const handleResetIssue = useCallback((newIssueId) => {
    store.setIssueId(newIssueId);
  }, [store]);
  return (
    <EditIssue
      visible={issueId}
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
      // resetIssue={(parentIssueId) => {
      //   handleResetIssue(parentIssueId);
      // }}
    />
  );
};

export default observer(IssueDetail);
