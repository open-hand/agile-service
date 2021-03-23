import React from 'react';
import { Observer } from 'mobx-react-lite';
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
            <Observer>
              {() => (
                <>
                  {store.blockList.map((block, index) => (
                    <Draggable key={block.key} draggableId={block.key} index={index}>
                      {(innerProvided: { innerRef: React.LegacyRef<HTMLDivElement> | undefined; draggableProps: JSX.IntrinsicAttributes & React.ClassAttributes<HTMLDivElement> & React.HTMLAttributes<HTMLDivElement>; }) => (
                        <div
                          ref={innerProvided.innerRef}
                          {...innerProvided.draggableProps}
                        >
                          <ReportBlock provided={innerProvided} index={index} data={block} />
                        </div>
                      )}
                    </Draggable>
                  ))}
                </>
              )}
            </Observer>
            {provided.placeholder}
          </div>
        )}
      </Droppable>
    </DragDropContext>
  );
};
export default BlockList;
