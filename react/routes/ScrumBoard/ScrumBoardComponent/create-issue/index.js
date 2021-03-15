import React from 'react';
import { observer } from 'mobx-react-lite';
import CreateIssue from '@/components/CreateIssue';
import scrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';

const ScrumBoardCreateIssue = ({ refresh }) => {
  const doingSprint = scrumBoardStore.didCurrentSprintExist ? scrumBoardStore.sprintNotClosedArray.find((item) => item.statusCode === 'started') : {};
  return (
    scrumBoardStore.getCreateIssueVisible ? (
      <CreateIssue
        visible={scrumBoardStore.getCreateIssueVisible}
        onCancel={() => {
          scrumBoardStore.setCreateIssueVisible(false);
        }}
        chosenSprint={scrumBoardStore.quickSearchObj?.sprintId || doingSprint?.sprintId}
        onOk={(res, requestData) => {
          scrumBoardStore.setCreateIssueVisible(false);
          // // 创建issue后刷新
          const { sprintId } = requestData;
          if (scrumBoardStore.getSprintId && String(sprintId) === String(scrumBoardStore.getSprintId)) {
            refresh(scrumBoardStore.getBoardList.get(scrumBoardStore.getSelectedBoard));
          }
        // if (res) {
        //   BacklogStore.refresh(false, false);
        // }
        }}
      />
    ) : null
  );
};
export default observer(ScrumBoardCreateIssue);
