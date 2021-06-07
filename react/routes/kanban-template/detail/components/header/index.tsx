import React, { useCallback, useContext } from 'react';
import classNames from 'classnames';
import { Icon, TextField, Modal } from 'choerodon-ui/pro';
import { DraggableProvided } from 'react-beautiful-dnd';
import TextEditToggle from '@/components/TextEditTogglePro';
import { MAX_LENGTH_KANBAN_COLUMN_NAME } from '@/constants/MAX_LENGTH';
import { IKanbanTemplateColumn } from '@/api';
import STATUS_COLOR from '@/constants/STATUS_COLOR';
import { observer } from 'mobx-react-lite';
import styles from './index.less';
import StatusLine from '../status-line';
import { Context } from '../../index';

interface HeaderProps extends React.HTMLAttributes<HTMLDivElement> {
  provided: DraggableProvided
  column: IKanbanTemplateColumn
}
const Header: React.FC<HeaderProps> = ({
  className,
  provided,
  column,
  ...otherProps
}) => {
  const { store } = useContext(Context);
  const handleSubmit = useCallback((newValue:string|null) => {
    if (newValue) {
      store.updateColumnName(column, newValue);
    }
  }, [column, store]);
  const handleDeleteClick = useCallback(() => {
    Modal.open({
      title: '删除列',
      children: '确认删除此列?',
      onOk: () => {
        store.deleteColumn(column);
      },
    });
  }, [column, store]);
  const { name, categoryCode } = column;
  return (
    <div className={classNames(styles.header, className)} {...otherProps}>
      <div className={styles.operation}>
        <Icon
          type="open_with"
          className={styles.operation__drag}
          {...provided.dragHandleProps}
        />
        <Icon
          type="delete_sweep-o"
          className={styles.operation__delete}
          onClick={handleDeleteClick}
        />
      </div>
      <div className={styles.statusName}>
        <TextEditToggle
          initValue={name}
          onSubmit={handleSubmit}
          editor={() => <TextField maxLength={MAX_LENGTH_KANBAN_COLUMN_NAME} valueChangeAction={'input' as any} />}
        >
          {name}
        </TextEditToggle>
      </div>
      <StatusLine color={STATUS_COLOR[categoryCode]?.[0]} />
    </div>
  );
};

export default observer(Header);
