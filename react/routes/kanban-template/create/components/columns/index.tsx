import React from 'react';
import classNames from 'classnames';
import { Droppable } from 'react-beautiful-dnd';
import styles from './index.less';
import Column from '../column';

interface ColumnProps extends React.HTMLAttributes<HTMLDivElement> {

}
const Columns: React.FC<ColumnProps> = ({
  className,
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
        {Array(10).fill(0).map((i, index) => <Column className={styles.column} index={index} />)}
        {provided.placeholder}
      </div>
    )}
  </Droppable>
);

export default Columns;
