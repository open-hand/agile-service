import React, { Fragment } from 'react';
import { observer } from 'mobx-react-lite';
import { Droppable } from 'react-beautiful-dnd';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import IssueList from './IssueList';
import SprintHeader from './SprintHeader';

function Sprint({ data, openCreateIssueModal }) {
  const { sprintId, expand } = data;
  const issueList = BacklogStore.getIssueListBySprintId(sprintId);
  return (
    <div style={{ width: '100%' }}>
      {/* <Droppable droppableId={`${sprintId}Trash`}>
        {(provided, snapshot) => (
          <div
            ref={provided.innerRef}
            style={{
              border: snapshot.isDraggingOver ? '1px solid green' : 'none',
            }}
          > */}
      <SprintHeader data={data} />
      {/* </div>
        )}
      </Droppable> */}

      {expand && (
        <>
          <IssueList sprintData={data} data={issueList} sprintId={sprintId} openCreateIssueModal={openCreateIssueModal} />
        </>
      )}
    </div>
  );
}

export default observer(Sprint);
