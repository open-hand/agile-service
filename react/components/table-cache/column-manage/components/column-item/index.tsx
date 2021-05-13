import React, { useContext, useMemo } from 'react';
import classNames from 'classnames';
import { Checkbox, Icon } from 'choerodon-ui';
import { Draggable, DraggingStyle, NotDraggingStyle } from 'react-beautiful-dnd';
import { observer } from 'mobx-react-lite';
import { useLockFn } from 'ahooks';
import { Option } from '../../Modal';
import styles from './index.less';
// import { Context } from '../..';

const grid = 0;
const getItemStyle = (isDragging: boolean, draggableStyle: DraggingStyle | NotDraggingStyle | undefined) => ({
  userSelect: 'none',
  margin: `${grid}px 0 `,
  ...draggableStyle,
} as const);
interface ColumnProps extends React.HTMLAttributes<HTMLDivElement> {
  index: number
  data: Option
  selected: boolean
  onSelectChange: (key: string, value: boolean) => void
}
const ColumnItem: React.FC<ColumnProps> = ({
  className,
  index,
  data,
  selected,
  onSelectChange,
  ...otherProps
}) => {
  // const { store } = useContext(Context);
  const draggableId = data.code;

  const handleCheckChange = useLockFn(async (e) => {
    onSelectChange(data.code, e.target.checked);
  });
  return (
    <Draggable
      index={index}
      draggableId={draggableId}
    >
      {(provided, snapshot) => (
        <div
          className={classNames(styles.item, className)}
          {...otherProps}
          ref={provided.innerRef}
          {...provided.draggableProps}
          style={getItemStyle(
            snapshot.isDragging,
            provided.draggableProps.style,
          )}
        >
          <Icon {...provided.dragHandleProps} type="baseline-drag_indicator" className={styles.handle} />
          <div className={styles.title}>{data.title}</div>
          <Checkbox className={styles.checkbox} checked={selected} onChange={handleCheckChange} />
        </div>
      )}
    </Draggable>
  );
};

export default observer(ColumnItem);
