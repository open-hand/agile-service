import React, { useCallback } from 'react';
import classNames from 'classnames';
import { Icon, TextField } from 'choerodon-ui/pro';
import { DraggableProvided } from 'react-beautiful-dnd';
import TextEditToggle from '@/components/TextEditTogglePro';
import { MAX_LENGTH_KANBAN_COLUMN_NAME } from '@/constants/MAX_LENGTH';
import styles from './index.less';
import StatusLine from '../status-line';

interface HeaderProps extends React.HTMLAttributes<HTMLDivElement> {
  provided: DraggableProvided
}
const Header: React.FC<HeaderProps> = ({
  className,
  provided,
  ...otherProps
}) => {
  const handleSubmit = useCallback(() => {

  }, []);
  return (
    <div className={classNames(styles.header, className)} {...otherProps}>
      <div className={styles.operation}>
        <Icon
          type="open_with"
          className={styles.operation__drag}
          {...provided.dragHandleProps}
        />
        <Icon
          type="delete"
          className={styles.operation__delete}
        />
      </div>
      <div className={styles.statusName}>
        <TextEditToggle
          initValue="待处理"
          onSubmit={handleSubmit}
          editor={() => <TextField maxLength={MAX_LENGTH_KANBAN_COLUMN_NAME} />}
        >
          待处理
        </TextEditToggle>
      </div>
      <StatusLine color="red" />
    </div>
  );
};

export default Header;
