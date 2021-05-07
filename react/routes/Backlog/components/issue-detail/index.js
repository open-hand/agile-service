import React, { useEffect, useImperativeHandle } from 'react';
import { observer } from 'mobx-react-lite';
import { versionApi } from '@/api';
import DetailContainer, { useDetail } from '@/components/detail-container';
import BacklogStore from '../../../../stores/project/backlog/BacklogStore';

const IssueDetail = ({ refresh, innerRef }) => {
  const [detailProps] = useDetail();
  const { open, close } = detailProps;
  const refreshIssueDetail = () => {
    close();
  };

  useEffect(() => {
    BacklogStore.setDetailProps(detailProps);
  }, [detailProps]);

  useImperativeHandle(innerRef, () => ({
    refreshIssueDetail,
  }));
  const visible = Object.keys(BacklogStore.getClickIssueDetail).length > 0;
  const { programId } = BacklogStore.getClickIssueDetail || {};
  const issueId = BacklogStore.getClickIssueId;
  useEffect(() => {
    if (visible) {
      open({
        path: 'issue',
        props: {
          issueId,
          programId,
          disabled: programId,
          applyType: programId ? 'program' : 'agile',
        },
        events: {
          update: () => {
            refresh();
          },
          delete: () => {
            if (BacklogStore.chosenEpic === BacklogStore.getClickIssueId) {
              BacklogStore.setChosenEpic('all');
            }
            BacklogStore.setClickIssueDetail({});
            BacklogStore.setIsLeaveSprint(false);
            refresh();
          },
          close: () => {
            BacklogStore.setClickIssueDetail({});
            BacklogStore.setIsLeaveSprint(false);
            BacklogStore.clearMultiSelected();
          },
          createVersion: () => {
            versionApi.loadAll().then((data2) => {
              const newVersion = [...data2];
              for (let index = 0, len = newVersion.length; index < len; index += 1) {
                newVersion[index].expand = false;
              }
              BacklogStore.setVersionData(newVersion);
            }).catch((error) => {
            });
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
