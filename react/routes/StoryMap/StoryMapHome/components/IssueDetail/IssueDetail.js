import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { getProjectId } from '@/utils/common';
import DetailContainer, { useDetail } from '@/components/detail-container';
import StoryMapStore from '../../../../../stores/project/StoryMap/StoryMapStore';

const IssueDetail = (props) => {
  const { refresh, isFullScreen, onChangeWidth } = props;
  const [detailProps] = useDetail();
  const { open, close } = detailProps;
  const handleCancel = () => {
    StoryMapStore.setClickIssue(null);
  };

  const handleDeleteIssue = () => {
    refresh();
    StoryMapStore.setClickIssue(null);
  };

  const { selectedIssueMap } = StoryMapStore;
  const visible = selectedIssueMap.size;
  const { programId, issueId } = selectedIssueMap.values().next().value || {};
  const programIssue = (programId && String(programId) !== String(getProjectId()));

  useEffect(() => {
    if (visible) {
      open({
        path: 'issue',
        props: {
          issueId,
          programId: programIssue ? programId : undefined,
          isFullScreen,
          disabled: isFullScreen || programIssue,
          applyType: programIssue ? 'program' : 'agile',
        // onChangeWidth={onChangeWidth}
        // onCancel={this.handleCancel}
        // onDeleteIssue={this.handleDeleteIssue}
        // onUpdate={refresh}
        },
        events: {
          update: refresh,
          delete: () => {
            handleDeleteIssue();
          },
          close: () => {
            handleCancel();
          },
          copy: () => {
            refresh();
            // handleIssueCopy();
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
