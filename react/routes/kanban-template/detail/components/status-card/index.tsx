import React, { useContext, useMemo } from 'react';
import classNames from 'classnames';
import { Radio } from 'choerodon-ui/pro';
import { Draggable, DraggingStyle, NotDraggingStyle } from 'react-beautiful-dnd';
import { IKanbanTemplateStatus } from '@/api';
import { observer } from 'mobx-react-lite';
import { useLockFn } from 'ahooks';
import StatusTypeTag from '@/components/tag/status-type-tag';
import styles from './index.less';
import { Context } from '../..';

const grid = 12;
const getItemStyle = (isDragging: boolean, draggableStyle: DraggingStyle | NotDraggingStyle | undefined) => ({
  userSelect: 'none',
  margin: `${grid}px 0 `,
  ...draggableStyle,
} as const);
interface ColumnProps extends React.HTMLAttributes<HTMLDivElement> {
  index: number
  columnId: string
  data: IKanbanTemplateStatus
}
const StatusCard: React.FC<ColumnProps> = ({
  className,
  index,
  columnId,
  data,
  ...otherProps
}) => {
  const { store } = useContext(Context);
  const draggableId = JSON.stringify({
    type: 'status',
    columnId,
    statusId: data.statusId,
  });
  const completed = useMemo(() => data.templateCompleted ?? data.completed, [data.completed, data.templateCompleted]);
  const handleCheckClick = useLockFn(async () => {
    await store.setStatusComplete(data, !completed);
  });
  return (
    <Draggable
      index={index}
      draggableId={draggableId}
    >
      {(provided, snapshot) => (
        <div
          className={classNames(styles.status_card, className)}
          {...otherProps}
          ref={provided.innerRef}
          {...provided.draggableProps}
          {...provided.dragHandleProps}
          style={getItemStyle(
            snapshot.isDragging,
            provided.draggableProps.style,
          )}
        >
          <StatusTypeTag
            mode="tag"
            code={data.categoryCode}
            name={data.name}
          />
          <br />
          <div role="none" className={styles.radio} onClick={handleCheckClick}>
            <Radio checked={completed}>设置已完成</Radio>
          </div>
        </div>
      )}
    </Draggable>
  );
};

export default observer(StatusCard);
