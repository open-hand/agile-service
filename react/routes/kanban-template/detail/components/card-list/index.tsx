import React from 'react';
import classNames from 'classnames';
import { Droppable } from 'react-beautiful-dnd';
import { IKanbanTemplateStatus } from '@/api';
import styles from './index.less';
import StatusCard from '../status-card';

interface CardListProps extends React.HTMLAttributes<HTMLDivElement> {
  columnId: string
  status: IKanbanTemplateStatus[]
}
const CardList: React.FC<CardListProps> = ({
  className,
  columnId,
  status,
  ...otherProps
}) => (
  <Droppable droppableId={columnId} direction="vertical" type="status_drop">
    {(provided, snapshot) => (
      <div
        className={classNames(styles.card_list, className)}
        {...otherProps}
        ref={provided.innerRef}
        {...provided.droppableProps}
        style={{
          width: '100%',
          background: snapshot.isDraggingOver
            ? 'rgba(26,177,111,0.08)'
            : 'unset',
        }}
      >
        {status.map((item, index) => <StatusCard key={item.statusId} data={item} index={index} columnId={columnId} />)}
        {provided.placeholder}
      </div>
    )}
  </Droppable>
);

export default CardList;
