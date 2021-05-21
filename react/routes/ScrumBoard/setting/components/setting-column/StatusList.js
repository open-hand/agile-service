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
              minHeight: 300,
            }}
          >
            {(data.subStatusDTOS.length > 0 || snapshot.isDraggingOver) ? data.subStatusDTOS.map((status, index) => (
              <StatusCard
                isDragDisabled={isDragDisabled}
                key={status.id}
                columnId={data.columnId}
                data={status}
                index={index}
                refresh={() => {}}
              />
            )) : (
              <>
                {
                  data.columnId !== '0' && (
                    <div className="c7n-scrumsetting-column-noStatusTip">缺少状态，至少为列分配一个状态，否则该列将不能显示在看板上</div>
                  )
                }
              </>
            )}
            {provided.placeholder}
          </div>
        )}
      </Droppable>
    </div>
  );
}
export default StatusList;
