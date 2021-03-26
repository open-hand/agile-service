import React from 'react';
import classNames from 'classnames';
import { Droppable } from 'react-beautiful-dnd';
import { IKanbanTemplateColumn } from '@/api';
import styles from './index.less';
import Column from '../column';

interface ColumnProps extends React.HTMLAttributes<HTMLDivElement> {
  columns: IKanbanTemplateColumn[]
}
const Columns: React.FC<ColumnProps> = ({
  className,
  columns,
  ...otherProps
}) => (
  <Droppable droppableId="columndrop" direction="horizontal" type="columndrop">
    {(provided) => (
      <div
        className={classNames(styles.columns, className)}
        {...otherProps}
        ref={provided.innerRef}
        {...provided.droppableProps}
      >
        {columns.map((column, index) => <Column key={column.columnId} data={column} className={styles.column} index={index} />)}
        {provided.placeholder}
      </div>
    )}
  </Droppable>
);

export default Columns;
