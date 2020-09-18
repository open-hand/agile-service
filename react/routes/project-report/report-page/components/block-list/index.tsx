import React from 'react';
import { observer } from 'mobx-react-lite';
import { DragDropContext, Droppable, Draggable } from 'react-beautiful-dnd';
import { useProjectReportContext } from '../../context';
import ReportBlock from '../report-block';

const BlockList: React.FC = () => {
  const { store } = useProjectReportContext();
  return (
    <DragDropContext onDragEnd={(result) => {
      if (!result.destination) {
        return;
      }
      store.sortBlock(result.source.index, result.destination.index);
    }}
    >
      <Droppable droppableId="droppable">
        {(provided) => (
          <div
            {...provided.droppableProps}
            ref={provided.innerRef}
          >
            {store.blockList.map((block, index) => (
              <Draggable key={block.key} draggableId={block.key} index={index}>
                {(innerProvided) => (
                  <div
                    ref={innerProvided.innerRef}
                    {...innerProvided.draggableProps}
                    {...innerProvided.dragHandleProps}
                  >
                    <ReportBlock index={index} data={block} />
                  </div>
                )}
              </Draggable>
            ))}
          </div>
        )}
      </Droppable>
    </DragDropContext>
  );
};
export default observer(BlockList);
