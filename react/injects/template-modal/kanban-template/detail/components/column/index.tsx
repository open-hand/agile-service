import React from 'react';
import classNames from 'classnames';
import { IKanbanTemplateColumn } from '@/api';
import styles from './index.less';
import Header from '../header';
import CardList from '../card-list';

interface ColumnProps extends React.HTMLAttributes<HTMLDivElement> {
  data: IKanbanTemplateColumn
  index: number
}
const Column: React.FC<ColumnProps> = ({
  className,
  index,
  data,
  ...otherProps
}) => (
  <div
    className={classNames(styles.column, className)}
    style={{ marginRight: 12 }}
  >
    <Header column={data} className={styles.column__header} />
    <CardList columnId={data.columnId} status={data.status} className={styles.column__content} />
  </div>
);

export default Column;
