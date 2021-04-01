import React from 'react';
import classNames from 'classnames';
import { IKanbanTemplateColumn } from '@/api';
import STATUS_COLOR from '@/constants/STATUS_COLOR';
import { observer } from 'mobx-react-lite';
import styles from './index.less';
import StatusLine from '../status-line';

interface HeaderProps extends React.HTMLAttributes<HTMLDivElement> {
  column: IKanbanTemplateColumn
}
const Header: React.FC<HeaderProps> = ({
  className,
  column,
  ...otherProps
}) => {
  const { name, categoryCode } = column;
  return (
    <div className={classNames(styles.header, className)} {...otherProps}>
      <div className={styles.statusName}>
        {name}
      </div>
      <StatusLine color={STATUS_COLOR[categoryCode]?.[0]} />
    </div>
  );
};

export default observer(Header);
