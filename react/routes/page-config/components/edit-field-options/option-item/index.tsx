import { FieldOption } from '@/common/types';
import { MAX_LENGTH_FIELD_CODE, MAX_LENGTH_FIELD_NAME } from '@/constants/MAX_LENGTH';
import { usePersistFn } from 'ahooks';
import { TextField, Button, Tooltip } from 'choerodon-ui/pro';
import { Popconfirm } from 'choerodon-ui';
import React, { memo } from 'react';
import {
  DraggableProvided,
} from 'react-beautiful-dnd';
import styles from './index.less';

interface OptionItemProps {
  data: FieldOption
  style?: React.CSSProperties
  isDragging: boolean
  provided: DraggableProvided
  editing?: FieldOption
  updateCurrentEdit?: (data: FieldOption | null) => void
  onUpdate?: (data: FieldOption) => Promise<any>
  onDelete?: (data: FieldOption) => Promise<any>

}
function getStyle({
  draggableStyle, virtualStyle, isDragging, data,
}: any) {
  let color = '#DDE7F2';
  if (isDragging) {
    color = '#DDE7F2';
  } else if (data.enabled) {
    color = '#FAFAFC';
  } else {
    color = '#F6F6F9';
  }
  const combined = {
    ...virtualStyle,
    ...draggableStyle,
  };

  const grid = 8;
  const height = isDragging ? combined.height : combined.height - grid;
  const result = {
    ...combined,
    left: isDragging ? combined.left : combined.left,
    width: isDragging
      ? draggableStyle.width
      : combined.width,
    marginBottom: grid,
    background: color,
    display: 'flex',
    alignItems: 'center',
    padding: '0 10px',
  };
  // eslint-disable-next-line no-restricted-globals
  if (!isNaN(height)) {
    result.height = height;
  }
  return result;
}
const OptionItem: React.FC<OptionItemProps> = ({
  data, style: virtualStyle, isDragging, provided, editing, updateCurrentEdit, onUpdate, onDelete,
}) => {
  const updateEditing = usePersistFn((newData) => {
    updateCurrentEdit && updateCurrentEdit(newData);
  });
  const update = usePersistFn(async (newData) => {
    onUpdate && await onUpdate(newData);
  });
  const handleEditClick = usePersistFn(() => {
    updateEditing(data);
  });
  const handleEnableClick = usePersistFn(async () => {
    await update({ ...data, enabled: true });
  });
  const handleDisableClick = usePersistFn(async () => {
    await update({ ...data, enabled: false });
  });
  const handleDeleteClick = usePersistFn(async () => {
    onDelete && await onDelete(data);
  });
  const handleCodeChange = usePersistFn((value) => {
    updateEditing({
      ...editing,
      code: value,
    });
  });
  const handleValueChange = usePersistFn((value) => {
    updateEditing({
      ...editing,
      value,
    });
  });
  const handleSave = usePersistFn(async () => {
    await update(editing);
    updateEditing(null);
  });
  const handleCancel = usePersistFn(async () => {
    updateEditing(null);
  });
  const renderEditor = usePersistFn(() => (
    <>
      <div className={styles.content}>
        <div className={styles.code}>
          <TextField
            value={editing?.code}
            onChange={handleCodeChange}
            maxLength={MAX_LENGTH_FIELD_CODE}
            valueChangeAction={'input' as any}
          />
        </div>
        <div className={styles.value}>
          <TextField
            value={editing?.value}
            onChange={handleValueChange}
            maxLength={MAX_LENGTH_FIELD_NAME}
            valueChangeAction={'input' as any}
          />
        </div>
      </div>
      <div className={styles.operate}>
        <Button onClick={handleSave}>确定</Button>
        <Button onClick={handleCancel}>取消</Button>
      </div>
    </>
  ));
  return (
    <div
      ref={provided.innerRef}
      {...provided.draggableProps}
      {...provided.dragHandleProps}
      style={{
        ...getStyle({
          draggableStyle: provided.draggableProps.style,
          virtualStyle,
          isDragging,
          data,
        }),
      }}
      className={styles.item}
    >
      {editing ? renderEditor() : (
        <>
          <div className={styles.content}>
            <div className={styles.code}>
              {data.code}
            </div>
            <div className={styles.value}>
              {data.value}
            </div>
          </div>
          <div className={styles.operate} key="read">
            <Button icon="edit-o" funcType={'flat' as any} onClick={handleEditClick} />
            <Tooltip title={data.enabled ? '禁用' : '启用'}>
              <Button
                icon={data.enabled ? 'block' : 'playlist_add_check'}
                funcType={'flat' as any}
                onClick={data.enabled ? handleDisableClick : handleEnableClick}
              />
            </Tooltip>
            <Popconfirm
              placement="top"
              title={`确认要删除 ${data.value} 吗？工作项上该字段值也会被清空。`}
              onConfirm={handleDeleteClick}
              okText="删除"
              cancelText="取消"
            >
              <Button icon="delete_sweep-o" funcType={'flat' as any} />
            </Popconfirm>
          </div>
        </>
      )}
    </div>
  );
};

export default memo(OptionItem);
