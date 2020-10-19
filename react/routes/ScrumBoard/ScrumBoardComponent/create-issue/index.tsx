import React from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import CreateIssue from '@/components/CreateIssue';
import scrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';

interface Props {
  refresh: (boardId: string) => void,
}
const ScrumBoardCreateIssue: React.FC<Props> = ({ refresh }) => (
  scrumBoardStore.getCreateIssueVisible ? (
    <CreateIssue
      visible={scrumBoardStore.getCreateIssueVisible}
      onCancel={() => {
        scrumBoardStore.setCreateIssueVisible(false);
      }}
      chosenSprint={scrumBoardStore.getSprintId}
      onOk={(res:any, requestData:any) => {
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
export default observer(ScrumBoardCreateIssue);
