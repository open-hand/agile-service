import React from 'react';
import classNames from 'classnames';
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
  <div
    className={classNames(styles.columns, className)}
    {...otherProps}
  >
    {columns.map((column, index) => <Column key={column.columnId} data={column} className={styles.column} index={index} />)}
  </div>
);

export default Columns;
