import React, { useContext, useCallback, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import DetailContainer, { useDetail } from '@/components/detail-container';
import { Issue } from '@/common/types';
import Context from '../../context';

interface Props {
  refresh: () => void
  onUpdate: (issue: Issue) => void
  onDelete: (issue: Issue) => void
  onDeleteSubIssue: (issue: Issue, subIssueId: string) => void
  onCreateSubIssue: (subIssue: Issue, parentIssueId: string) => void
  onCopyIssue: (issue: Issue, issueId: string) => void
  onTransformType: (newIssue: Issue, oldIssue: Issue) => void
  onChangeParent: (newIssue: Issue, oldIssue: Issue) => void
  onLinkIssue: ({ influenceIssueIds }:{ influenceIssueIds?: string[] }) => void
}
const IssueDetail: React.FC<Props> = ({
  refresh, onUpdate, onDelete, onDeleteSubIssue, onCreateSubIssue, onCopyIssue, onTransformType, onChangeParent, onLinkIssue,
}) => {
  const { store } = useContext(Context);
  const { issueId } = store;
  const handleResetIssue = useCallback((newIssueId) => {
    store.setIssueId(newIssueId);
  }, [store]);
  const [detailProps] = useDetail();

  useEffect(() => {
    store.setDetailProps(detailProps);
  }, [detailProps, store]);
  const { open, close } = detailProps;

  const visible = issueId;
  useEffect(() => {
    if (visible) {
      open({
        path: 'issue',
        props: {
          issueId,
        },
        events: {
          update: onUpdate,
          delete: (issue) => {
            onDelete(issue);
            handleResetIssue(null);
          },
          deleteSubIssue: onDeleteSubIssue,
          createSubIssue: onCreateSubIssue,
          close: () => {
            handleResetIssue(null);
          },
          copy: onCopyIssue,
          transformType: onTransformType,
          changeParent: onChangeParent,
          linkIssue: onLinkIssue,
        },
      });
    } else {
      close();
    }
  }, [visible, issueId, open, refresh,
    handleResetIssue, close, onUpdate, onDelete,
    onDeleteSubIssue, onCreateSubIssue, onCopyIssue,
    onTransformType, onChangeParent, onLinkIssue]);
  return (
    <DetailContainer {...detailProps} />
  );
};

export default observer(IssueDetail);
