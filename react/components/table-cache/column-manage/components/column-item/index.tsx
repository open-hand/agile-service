import React from 'react';
import classNames from 'classnames';
import { Checkbox, Icon } from 'choerodon-ui';
import { Tooltip } from 'choerodon-ui/pro';
import { Draggable, DraggingStyle, NotDraggingStyle } from 'react-beautiful-dnd';
import { observer } from 'mobx-react-lite';
import { useLockFn } from 'ahooks';
import { CustomColumnManageProps, Option } from '../../Modal';
import styles from './index.less';

const grid = 0;
const getItemStyle = (isDragging: boolean, draggableStyle: DraggingStyle | NotDraggingStyle | undefined) => ({
  userSelect: 'none',
  margin: `${grid}px 0 `,
  ...draggableStyle,
} as const);
interface ColumnProps extends React.HTMLAttributes<HTMLDivElement> {
  index: number
  data: Option
  tooltip: boolean
  selected: boolean
  renderItem: Exclude<CustomColumnManageProps['renderItem'], undefined>
  onSelectChange: (key: string, value: boolean) => void
}
const ColumnItem: React.FC<Pick<ColumnProps, 'data' | 'selected' | 'onSelectChange' | 'tooltip'>> = ({
  data,
  selected,
  onSelectChange,
  tooltip,
}) => {
  const handleCheckChange = useLockFn(async (e) => {
    onSelectChange(data.code, e.target.checked);
  });
  return (
    <>
      {tooltip ? (
        <Tooltip title={data.title}>
          <div className={styles.title}>{data.title}</div>
        </Tooltip>
      ) : <div className={styles.title}>{data.title}</div>}
      <Checkbox className={styles.checkbox} checked={selected} onChange={handleCheckChange} />
    </>
  );
};
const DragableColumnItem: React.FC<ColumnProps> = ({
  className,
  index,
  data,
  selected,
  onSelectChange,
  tooltip,
  renderItem,
  ...otherProps
}) => {
  const draggableId = data.code;
  const item = <ColumnItem tooltip={tooltip} data={data} selected={selected} onSelectChange={onSelectChange} />;
  if (data.disabled) {
    return null;
  }
  return (
    <Draggable
      index={index}
      draggableId={draggableId}
    >
      {(provided, snapshot) => {
        const dom = (
          <div
            className={classNames(styles.item, className)}
            {...otherProps}
            ref={provided.innerRef}
            {...provided.draggableProps}
            {...provided.dragHandleProps}
            style={getItemStyle(
              snapshot.isDragging,
              provided.draggableProps.style,
            )}
          >
            <Icon type="baseline-drag_indicator" className={styles.handle} />
            {item}
          </div>
        );
        return renderItem(dom, {
          data, selected, tooltip, index,
        });
      }}
    </Draggable>
  );
};

export default observer(DragableColumnItem);
