import React, { useContext, useCallback, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import DetailContainer, { useDetail } from '@/components/detail-container';
import Context from '../../context';

interface Props {
  refresh: () => void
}
const IssueDetail: React.FC<Props> = ({ refresh }) => {
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
          update: () => {
            refresh();
          },
          delete: () => {
            handleResetIssue(null);
            refresh();
          },
          close: () => {
            handleResetIssue(null);
          },
          copy: () => {
            refresh();
          },
        },
      });
    } else {
      close();
    }
  }, [visible, issueId, open, refresh, handleResetIssue, close]);
  return (
    <DetailContainer {...detailProps} />
  );
};

export default observer(IssueDetail);
