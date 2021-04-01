import React from 'react';
import classNames from 'classnames';
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
  <div
    className={classNames(styles.card_list, className)}
  >
    {status.map((item, index) => <StatusCard key={item.statusId} data={item} index={index} columnId={columnId} />)}

  </div>
);

export default CardList;
