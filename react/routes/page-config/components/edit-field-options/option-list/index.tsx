import ReactDOM from 'react-dom';
import { fieldApi } from '@/api';
import { FieldOption } from '@/common/types';
import { usePersistFn } from 'ahooks';
import { Button } from 'choerodon-ui/pro';
import React, {
  forwardRef, useImperativeHandle, useState,
} from 'react';
import {
  DragDropContext, Droppable, Draggable, DropResult,
} from 'react-beautiful-dnd';
import { List, AutoSizer } from 'react-virtualized';
import { Choerodon } from '@choerodon/boot';
import reorder from '@/utils/reorder';
import OptionItem from '../option-item';
import AddItem from '../add-item';
import styles from './index.less';

export interface OptionListProps {
  fieldOptions: FieldOption[]
  fieldId: string
}
export interface OptionListRef {
  fieldOptions: FieldOption[]
}
const OptionList: React.ForwardRefRenderFunction<OptionListRef, OptionListProps> = ({
  fieldOptions: propsFieldOptions, fieldId,
}, ref) => {
  const [currentEdit, setCurrentEdit] = useState<FieldOption | null>(null);
  const [currentAdd, setCurrentAdd] = useState(false);
  const [fieldOptions, setFieldOptions] = useState<FieldOption[]>(propsFieldOptions);
  useImperativeHandle(ref, () => ({ fieldOptions }));
  const refresh = usePersistFn(async () => {
    const res = await fieldApi.getFieldOptions(fieldId, undefined, 1, 0);
    setFieldOptions(res.list);
  });
  const handleDragEnd = usePersistFn(async (result: DropResult) => {
    const { source, destination } = result;
    // 拖拽到边框外
    if (!destination) {
      return;
    }
    const sourceItem = fieldOptions[source.index];
    // 排序
    const items = reorder(
      fieldOptions,
      source.index,
      destination.index,
    );
    // 本地更新
    setFieldOptions(items);
    try {
      await fieldApi.updateFieldOption(fieldId, sourceItem.id, {
        ...sourceItem,
        sequence: destination.index,
      });
    } catch (error) {
      await refresh();
    }
  });
  const hasSameValue = usePersistFn((id, value) => fieldOptions.some((o) => o.id !== id && o.value === value));
  const hasSameCode = usePersistFn((id, code) => fieldOptions.some((o) => o.id !== id && o.code === code));
  const validate = usePersistFn((newValue) => {
    const { id } = newValue;
    if (hasSameValue(id, newValue.value)) {
      Choerodon.prompt('值不能重复');
      throw new Error('值不能重复');
    }
    if (hasSameCode(id, newValue.code)) {
      Choerodon.prompt('编码不能重复');
      throw new Error('编码不能重复');
    }
  });
  const handleUpdate = usePersistFn(async (newValue) => {
    const { id } = newValue;
    validate(newValue);
    await fieldApi.updateFieldOption(fieldId, id, newValue);
    // 本地更新
    setFieldOptions((o) => o.map((item) => (item.id === id ? newValue : item)));
  });
  const handleCreate = usePersistFn(async (item) => {
    validate(item);
    const res = await fieldApi.createFieldOption(fieldId, item);
    setFieldOptions((old) => ([
      res,
      ...old,
    ]));
    setCurrentAdd(false);
  });
  const handleDelete = usePersistFn(async (item) => {
    await fieldApi.deleteFieldOption(fieldId, item.id);
    setFieldOptions((old) => old.filter((i) => i.id !== item.id));
  });
  const renderClone = usePersistFn((provided, snapshot, rubric) => {
    const { index } = rubric.source;
    const item = fieldOptions[index];
    if (!item) {
      return (
        <div
          ref={provided.innerRef}
          {...provided.draggableProps}
          {...provided.dragHandleProps}
        />
      );
    }
    return (
      <OptionItem
        data={item}
        isDragging={snapshot.isDragging}
        provided={provided}
      />
    );
  });
  const handleAddClick = usePersistFn(() => {
    setCurrentAdd(true);
  });

  const handleCancelAdd = usePersistFn(() => {
    setCurrentAdd(false);
  });

  return (
    <DragDropContext onDragEnd={handleDragEnd}>
      <div className={styles.container}>
        <div className={styles.header}>
          <div className={styles.title}>
            值
          </div>
          <div className={styles.title}>
            显示值
          </div>
        </div>
        <Droppable
          droppableId="droppable"
          mode="virtual"
          renderClone={renderClone}
        >
          {(p) => (
            <div
              ref={p.innerRef}
              className={styles.list}
            >
              <AutoSizer>
                {({ width, height }) => (
                  <List
                    ref={(listRef) => {
                      // 如果不设置ref，拖拽时滚动条会失效
                      // react-virtualized has no way to get the list's ref that I can so
                      // So we use the `ReactDOM.findDOMNode(ref)` escape hatch to get the ref
                      if (listRef) {
                        // eslint-disable-next-line react/no-find-dom-node
                        const whatHasMyLifeComeTo = ReactDOM.findDOMNode(listRef);
                        if (whatHasMyLifeComeTo instanceof HTMLElement) {
                          p.innerRef(whatHasMyLifeComeTo);
                        }
                      }
                    }}
                    height={height ?? 200}
                    overscanRowCount={5}
                    rowCount={fieldOptions.length}
                    rowHeight={60}
                    rowRenderer={({ index, style }) => {
                      const item = fieldOptions[index];
                      return (
                        <Draggable
                          key={item.id}
                          draggableId={item.id}
                          index={index}
                          disableInteractiveElementBlocking={false}
                        >
                          {(provided, snapshot) => (
                            <OptionItem
                              data={item}
                              editing={item.id === currentEdit?.id ? currentEdit : undefined}
                              updateCurrentEdit={setCurrentEdit}
                              onUpdate={handleUpdate}
                              onDelete={handleDelete}
                              style={style}
                              isDragging={snapshot.isDragging}
                              provided={provided}
                            />
                          )}
                        </Draggable>
                      );
                    }}
                    width={width ?? 200}
                  />
                )}
              </AutoSizer>
            </div>
          )}
        </Droppable>
        <div className={styles.footer}>
          {currentAdd && (
            <AddItem
              onSubmit={handleCreate}
              onCancel={handleCancelAdd}
            />
          )}
          <Button
            onClick={handleAddClick}
            disabled={currentAdd}
            style={{ marginTop: 16 }}
            icon="playlist_add"
          >
            添加
          </Button>
        </div>
      </div>
    </DragDropContext>
  );
};

export default forwardRef(OptionList);
