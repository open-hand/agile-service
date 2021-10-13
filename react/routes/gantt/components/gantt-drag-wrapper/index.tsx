/* eslint-disable no-underscore-dangle */
import React, {
  useState, useCallback, useRef, useContext,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  merge,
} from 'lodash';
import {
  usePersistFn,
} from 'ahooks';
import {
  DragDropContext, Draggable, Droppable,
} from 'react-beautiful-dnd';
import type {
  DraggableStateSnapshot, DragUpdate, DropResult, ResponderProvided,
} from 'react-beautiful-dnd';
import type { Gantt } from '@choerodon/gantt';
import Context from '../../context';

interface IGanttGroupDragWrapperProps {
  renderClone: (record: Gantt.Record) => React.ReactElement
  onDragEnd: (sourceBar: Gantt.Bar, destinationBar: Gantt.Bar) => void
}
const isDisableDrag = (bar: Gantt.Bar) => {
  const { record } = bar;
  if (record.disabledDrag || record.create) {
    return true;
  }
  return false;
};
/**
 * 拖拽的元素是否能放置到目标位置
 */
function isDragRowDrop(bar: Gantt.Bar, destinationBar?: Gantt.Bar) {
  if (bar._depth === destinationBar?._depth && bar._parent?.key === destinationBar?._parent?.key) {
    return true;
  }
  return false;
}

function isNeedDraggingStyle(destinationBar: Gantt.Bar, bar?: Gantt.Bar): boolean {
  if (bar?.key === destinationBar.key || bar?._parent?.key === destinationBar.key) {
    return true;
  }
  if (bar && bar._depth > destinationBar!._depth && bar?._parent) {
    return isNeedDraggingStyle(destinationBar, bar?._parent._bar!);
  }
  return false;
}
function getDestinationBar(sourceDepth: number, bar?: Gantt.Bar): Gantt.Bar | undefined {
  if (!bar || bar._depth < sourceDepth) {
    return undefined;
  }
  if (bar._depth > sourceDepth && bar._parent?._bar) {
    return getDestinationBar(sourceDepth, bar._parent?._bar!);
  }
  return bar._depth === sourceDepth ? bar : undefined;
}
const GanttGroupDragWrapper: React.FC<IGanttGroupDragWrapperProps> = ({
  children, renderClone, onDragEnd,
}) => {
  const {
    store,
  } = useContext(Context);
  const [{ source: draggingBar, destination: draggingBarDestinationBar }, setDraggingBar] = useState<{ source?: Gantt.Bar, destination?: Gantt.Bar }>({} as any);
  const draggingBarDestinationBarRef = useRef<Gantt.Bar>();
  const draggingStyle = useRef<React.CSSProperties>();
  const renderTableBody = useCallback((Component: React.ReactElement, ganttStore: any) => (
    <Droppable
      droppableId="table"
      direction="vertical"
      mode="virtual"
      renderClone={(provider, snapshot, rubric) => {
        const record = store.ganttRef.current?.flattenData[rubric.source.index]?.record;
        return (
          <div
            ref={provider.innerRef}
            {...provider.draggableProps}
            {...provider.draggableProps}
          >
            {record && renderClone(record)}
          </div>
        );
      }}
    >
      {(provided, dropSnapshot) => {
        if (dropSnapshot.isDraggingOver) {
          ganttStore.setTableTranslateX(0);
        }
        return React.cloneElement(Component, {
          ref: (r: any) => {
            // eslint-disable-next-line no-param-reassign
            (Component as any).ref.current = r;
            provided.innerRef(r);
          },
          ...provided.droppableProps,
          style: { ...Component.props.style, background: dropSnapshot.isDraggingOver ? '#F1F3F6' : undefined } as React.CSSProperties,
        });
      }}
    </Droppable>
  ), [renderClone, store.ganttRef]);

  /**
   * 获取拖拽行样式
   */
  const getDragRowStyle = useCallback((style: React.CSSProperties, bar: Gantt.Bar, snapshot: DraggableStateSnapshot, dragStyle?: React.CSSProperties): React.CSSProperties => {
    const baseStyle: React.CSSProperties = { ...style, ...dragStyle };
    if (draggingBar && draggingBarDestinationBar) {
      // baseStyle.cursor = 'not-allowed';
      if (isNeedDraggingStyle(draggingBarDestinationBar, bar)) {
        merge(baseStyle, draggingStyle.current);
      }
      return baseStyle;
    }
    return style;
  }, [draggingBar, draggingBarDestinationBar]);
  const renderTableRow = useCallback((row: React.ReactElement, bar: Gantt.Bar) => (
    <Draggable
      key={`drag-${bar.absoluteIndex}`}
      draggableId={String(bar.absoluteIndex)}
      isDragDisabled={isDisableDrag(bar)}
      index={bar.absoluteIndex}
    >
      {(provided, snapshot) => React.cloneElement(row, {
        ...provided.dragHandleProps,
        ...provided.draggableProps,
        ref: provided.innerRef,
        style: getDragRowStyle(row.props?.style, bar, snapshot, provided.draggableProps?.style),
      })}
    </Draggable>
  ), [getDragRowStyle]);
  const handleDragStart = usePersistFn((initial: any) => {
    draggingBarDestinationBarRef.current = undefined;
    const dragBar = store.ganttRef.current?.flattenData[Number(initial.draggableId)];
    if (dragBar && dragBar._childrenCount > 0) {
      store.ganttRef.current?.setRowCollapse(Number(initial.draggableId), true);
    }
    setDraggingBar({ source: dragBar });
  });

  const handleDragEnd = useCallback((result: DropResult, provider: ResponderProvided) => {
    setDraggingBar({} as any);
    if (!result.destination || result.destination.index === result.source.index) {
      return;
    }
    const flattenData = store.ganttRef.current?.flattenData || [];
    const destinationData = draggingBarDestinationBarRef.current!;
    const sourceData = flattenData[result.source.index];
    if (!destinationData || !sourceData || !isDragRowDrop(sourceData, destinationData)) {
      return;
    }
    console.log(sourceData.record.summary, 'move--->', destinationData.record.summary);
    onDragEnd(sourceData, destinationData);
  }, [onDragEnd, store.ganttRef]);
  const handleDragUpdate = useCallback((initial: DragUpdate, provided: ResponderProvided) => {
    setDraggingBar((oldValue) => {
      if (!oldValue.source || oldValue.destination?.absoluteIndex === initial.destination?.index) {
        return oldValue;
      }
      if (!initial.destination) {
        draggingBarDestinationBarRef.current = undefined;
        return { source: oldValue.source, destination: undefined };
      }
      const destinationBar = store.ganttRef.current?.flattenData[initial.destination.index];
      // 移动是同层级进行移动，因此这里获取的目标节点应该是同层级的,当目标节点为高层级别的，则无效
      const newDestination = getDestinationBar(oldValue.source._depth, destinationBar);
      console.log(oldValue.source.record.summary, oldValue.source._depth, 'drag===>', newDestination, newDestination?.record.summary);
      draggingStyle.current = newDestination && { transform: `translate(0px, ${oldValue.source.absoluteIndex > newDestination.absoluteIndex ? 34 : -34}px)` };
      draggingBarDestinationBarRef.current = newDestination;

      return { source: oldValue.source, destination: newDestination };
    });
  }, [store.ganttRef]);

  return (
    <DragDropContext
      onBeforeCapture={handleDragStart}
      onDragEnd={handleDragEnd}
      onDragUpdate={handleDragUpdate}
    >
      {React.cloneElement(children as React.ReactElement, { components: { tableBody: renderTableBody, tableRow: renderTableRow } })}
    </DragDropContext>
  );
};
export default observer(GanttGroupDragWrapper);
