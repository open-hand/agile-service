import React from 'react';
import classNames from 'classnames';
import { Draggable, DraggingStyle, NotDraggingStyle } from 'react-beautiful-dnd';
import styles from './index.less';
import Header from '../header';
import CardList from '../card-list';

const grid = 12;
const getItemStyle = (index:number, isDragging: boolean, draggableStyle: DraggingStyle | NotDraggingStyle | undefined) => ({
  userSelect: 'none',
  margin: `0 ${grid}px 0 0`,
  ...draggableStyle,
} as const);
interface ColumnProps extends React.HTMLAttributes<HTMLDivElement> {
  index: number
}
const Column: React.FC<ColumnProps> = ({
  className,
  index,
  ...otherProps
}) => {
  const draggableId = JSON.stringify({
    type: 'column',
    columnId: index,
  });
  return (
    <Draggable
      index={index}
      draggableId={draggableId}
    >
      {(provided, snapshot) => (
        <div
          className={classNames(styles.column, className)}
          {...otherProps}
          ref={provided.innerRef}
          {...provided.draggableProps}
          style={getItemStyle(
            index,
            snapshot.isDragging,
            provided.draggableProps.style,
          )}
        >
          <Header className={styles.column__header} provided={provided} />
          <CardList className={styles.column__content} columnId={draggableId} />
        </div>
      )}
    </Draggable>
  );
};

export default Column;
