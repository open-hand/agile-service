import React from 'react';
import {observer} from 'mobx-react-lite';
import {Droppable} from 'react-beautiful-dnd';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import IssueList from './IssueList';
import SprintHeader from './SprintHeader';
import IssueItem from './IssueItem';

function Sprint({ data, openCreateIssueModal, sprintIndex, refresh }) {
  const { sprintId, expand } = data;
  const issueList = BacklogStore.getIssueListBySprintId(sprintId);
  return (
    <div style={{ width: '100%' }}>
      <Droppable
        droppableId={String(sprintId)}
        mode="virtual"
        isDropDisabled={BacklogStore.getIssueCantDrag}
        renderClone={(provided, snapshot, rubric) => (
          <IssueItem
            provided={provided}
            isDragging={snapshot.isDragging}
            issue={issueList[rubric.source.index]}
            sprintId={sprintId}
            style={{ margin: 0 }}
          />
        )}
      >
        {(provided, snapshot) => (
          <div
            ref={provided.innerRef}
          >
            <div
              style={{
                border: !expand && snapshot.isDraggingOver ? '1px dashed green' : 'none',
              }}
            >
              <SprintHeader data={data} sprintIndex={sprintIndex} refresh={refresh} />
            </div>
            {expand && (
              <>
                <IssueList
                  sprintIndex={sprintIndex}
                  sprintData={data}
                  data={issueList}
                  sprintId={sprintId}
                  openCreateIssueModal={openCreateIssueModal}
                  provided={provided}
                  snapshot={snapshot}
                />
              </>
            )}
          </div>
        )}
      </Droppable>
    </div>
  );
}

export default observer(Sprint);
