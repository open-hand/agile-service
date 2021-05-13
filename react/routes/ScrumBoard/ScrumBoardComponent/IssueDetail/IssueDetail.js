import React, { useEffect } from 'react';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import { observer } from 'mobx-react-lite';
import DetailContainer, { useDetail } from '@/components/detail-container';

const IssueDetail = ({ refresh }) => {
  const [detailProps] = useDetail();
  const { open, close } = detailProps;

  useEffect(() => {
    ScrumBoardStore.setDetailProps(detailProps);
  }, [detailProps]);

  const handleIssueCopy = ({ issueId }) => {
    refresh();
    ScrumBoardStore.clickedOnce('0', {
      issueId,
    });
  };

  const issueId = ScrumBoardStore.getCurrentClickId;
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
  }, [visible, issueId]);
  return (
    <DetailContainer {...detailProps} />
  );
};

export default observer(IssueDetail);
