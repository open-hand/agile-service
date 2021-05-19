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
        {(status.length > 0 || snapshot.isDraggingOver) ? status.map((item, index) => <StatusCard key={item.statusId} data={item} index={index} columnId={columnId} />) : (
          <>
            {
            columnId !== '0' && <div className={styles.noStatus_tip}>缺少状态，至少为列分配一个状态，否则该列将不能显示在看板上</div>
            }
          </>
        )}
        {provided.placeholder}
      </div>
    )}
  </Droppable>
);

export default CardList;
