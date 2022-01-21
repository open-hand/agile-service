import React, { useEffect, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import DetailContainer, { useDetail } from '@/components/detail-container';

const IssueDetail = ({ refresh }) => {
  const [detailProps] = useDetail();
  const { open, close } = detailProps;

  useEffect(() => {
    ScrumBoardStore.setDetailProps(detailProps);
  }, [detailProps]);

  const handleIssueCopy = useCallback(({ issueId } = {}) => {
    refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
    if (issueId) {
      ScrumBoardStore.clickedOnce('0', {
        issueId,
      });
    }
  }, [refresh]);

  const issueId = ScrumBoardStore.getCurrentClickId;
  const visible = issueId;
  useEffect(() => {
    if (visible) {
      open({
        path: 'issue',
        props: {
          issueId,
          applyType: 'agile',
          forwardedRef: ScrumBoardStore.editRef,
        },
        events: {
          update: () => {
            refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
          },
          delete: () => {
            ScrumBoardStore.resetClickedIssue();
            refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
          },
          close: () => {
            ScrumBoardStore.resetClickedIssue();
          },
          copy: () => {
            handleIssueCopy();
          },
        },
      });
    } else {
      close();
    }
  }, [visible, issueId, open, refresh, close, handleIssueCopy]);
  return (
    <DetailContainer {...detailProps} />
  );
};

export default observer(IssueDetail);
