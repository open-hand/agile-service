import React from 'react';
import classNames from 'classnames';
import { Droppable } from 'react-beautiful-dnd';
import styles from './index.less';
import StatusCard from '../status-card';

interface CardListProps extends React.HTMLAttributes<HTMLDivElement> {
  columnId: string
}
const CardList: React.FC<CardListProps> = ({
  className,
  columnId,
  ...otherProps
}) => (
  <Droppable droppableId={`${columnId}-status_drop`} direction="vertical" type="status_drop">
    {(provided) => (
      <div
        className={classNames(styles.card_list, className)}
        {...otherProps}
        ref={provided.innerRef}
        {...provided.droppableProps}
      >
        {Array(20).fill(0).map((i, index) => <StatusCard index={index} columnId={columnId} />)}
        {provided.placeholder}
      </div>
    )}
  </Droppable>
);

export default CardList;
