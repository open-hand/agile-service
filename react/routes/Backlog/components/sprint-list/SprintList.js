import React from 'react';
import {DragDropContext} from 'react-beautiful-dnd';
import {observer} from 'mobx-react-lite';
import 'react-virtualized/styles.css';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import Sprint from './Sprint';

function SprintList({ openCreateIssueModal, refresh }) {
  const { showPlanSprint } = BacklogStore;
  const sprintList = showPlanSprint ? BacklogStore.getSprintData : BacklogStore.getSprintData.filter((sprint) => sprint.statusCode !== 'sprint_planning');

  return (
    <DragDropContext onBeforeCapture={BacklogStore.onBeforeCapture} onDragEnd={BacklogStore.onDragEnd} onDragStart={BacklogStore.onDragStart}>
      {sprintList.map((sprint, sprintIndex) => <Sprint sprintIndex={sprintIndex} data={sprint} key={sprint.sprintId} openCreateIssueModal={openCreateIssueModal} refresh={refresh} />)}
    </DragDropContext>
  );
}

export default observer(SprintList);
