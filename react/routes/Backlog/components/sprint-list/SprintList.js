import React from 'react';
import { DragDropContext } from 'react-beautiful-dnd';
import { observer } from 'mobx-react-lite';
import 'react-virtualized/styles.css';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import Sprint from './Sprint';
import NoneSprint from './NoneSprint';

function SprintList() {
  const { showPlanSprint } = BacklogStore;
  const sprintList = showPlanSprint ? BacklogStore.getSprintData : BacklogStore.getSprintData.filter(sprint => sprint.statusCode !== 'sprint_planning');
  const loading = BacklogStore.getSpinIf;
  
  return sprintList.length === 0 || loading
    ? <NoneSprint loading={loading} /> 
    : (
      <DragDropContext onDragEnd={BacklogStore.onDragEnd} onDragStart={BacklogStore.onDragStart}>
        {sprintList.map(sprint => <Sprint data={sprint} key={sprint.sprintId} />)}
      </DragDropContext>
    );
}

export default observer(SprintList);
