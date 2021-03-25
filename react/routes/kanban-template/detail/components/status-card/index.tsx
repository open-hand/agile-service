import React from 'react';
import classNames from 'classnames';
import { Draggable, DraggingStyle, NotDraggingStyle } from 'react-beautiful-dnd';
import styles from './index.less';

const grid = 12;
const getItemStyle = (isDragging: boolean, draggableStyle: DraggingStyle | NotDraggingStyle | undefined) => ({
  userSelect: 'none',
  margin: `${grid}px 0 `,
  ...draggableStyle,
} as const);
interface ColumnProps extends React.HTMLAttributes<HTMLDivElement> {
  index: number
  columnId: string
}
const StatusCard: React.FC<ColumnProps> = ({
  className,
  index,
  columnId,
  ...otherProps
}) => {
  const draggableId = JSON.stringify({
    type: 'status',
    columnId,
    statusId: index,
  });
  return (
    <Draggable
      index={index}
      draggableId={draggableId}
    >
      {(provided, snapshot) => (
        <div
          className={classNames(styles.status_card, className)}
          {...otherProps}
          ref={provided.innerRef}
          {...provided.draggableProps}
          {...provided.dragHandleProps}
          style={getItemStyle(
            snapshot.isDragging,
            provided.draggableProps.style,
          )}
        >
          StatusCard
        </div>
      )}
    </Draggable>
  );
};

export default StatusCard;
