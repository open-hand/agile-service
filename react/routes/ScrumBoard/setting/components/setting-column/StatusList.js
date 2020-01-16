import React from 'react';
import { Droppable } from 'react-beautiful-dnd';
import StatusCard from '../StatusCard/StatusCard';

function StatusList({ data, isDragDisabled }) {
  return (
    <div className="c7n-scrumsetting-columnDrop">
      <Droppable
        type="status"
        droppableId={`${data.categoryCode || 'unset'},${data.columnId}`}
      >
        {(provided, snapshot) => (
          <div
            ref={provided.innerRef}
            style={{
              background: snapshot.isDraggingOver
                ? 'rgba(26,177,111,0.08)' : 'unset',
              height: '100%',
            }}
          >
            {data.subStatusDTOS.map((status, index) => (
              <StatusCard
                isDragDisabled={isDragDisabled}
                key={status.id}
                columnId={data.columnId}
                data={status}
                index={index}
                refresh={() => {}}
              />
            ))}
            {provided.placeholder}
          </div>
        )}
      </Droppable>
    </div>
  );
}
export default StatusList;
