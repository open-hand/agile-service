import React, {
  useCallback, useState, useRef, useMemo,
} from 'react';
import { usePersistFn } from 'ahooks';
import { observer } from 'mobx-react-lite';
import AutoScroller from './AutoScroller';

interface Size {

}
interface DragResizeProps extends React.HTMLProps<HTMLDivElement> {
  onResize: ({ width, x }: { width: number, x: number }) => void
  onResizeEnd: () => void
  minWidth: number
  type: 'left' | 'right' | 'move'
  grid?: number
  scroller?: HTMLElement
  defaultSize: {
    width: number
    x: number
  }
}
const snap = (n: number, size: number): number => Math.round(n / size) * size;
const DragResize: React.FC<DragResizeProps> = ({
  type, onResize, onResizeEnd, minWidth, grid, defaultSize: { x: defaultX, width: defaultWidth }, scroller, children, ...otherProps
}) => {
  const [resizing, setResizing] = useState(false);
  const autoScroll = useMemo(() => new AutoScroller({ scroller }), [scroller]);
  const positionRef = useRef({
    clientX: 0,
    width: defaultWidth,
    x: defaultX,
  });
  const handleMouseMove = usePersistFn((event: MouseEvent) => {
    const distance = event.clientX - positionRef.current.clientX + autoScroll.autoScrollPos;
    switch (type) {
      case 'left': {
        let width = positionRef.current.width - distance;
        if (minWidth !== undefined) {
          width = Math.max(width, minWidth);
        }
        if (grid) {
          width = snap(width, grid);
        }
        const pos = width - positionRef.current.width;
        const x = positionRef.current.x - pos;
        onResize({ width, x });
        break;
      }
      case 'right': {
        let width = positionRef.current.width + distance;
        if (minWidth !== undefined) {
          width = Math.max(width, minWidth);
        }
        if (grid) {
          width = snap(width, grid);
        }
        const pos = width - positionRef.current.width;
        const x = positionRef.current.x + pos;
        onResize({ width, x });
        break;
      }
      case 'move': {
        const { width } = positionRef.current;
        const x = positionRef.current.x + distance;
        onResize({ width, x });
        break;
      }
    }
  });
  const handleMouseUp = usePersistFn(() => {
    autoScroll.stop();
    window.removeEventListener('mousemove', handleMouseMove);
    window.removeEventListener('mouseup', handleMouseUp);
    setResizing(false);
    onResizeEnd();
  });
  const handleMouseDown = usePersistFn((event: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
    event.stopPropagation();
    if (scroller) {
      autoScroll.start();
    }
    positionRef.current.clientX = event.clientX;
    positionRef.current.x = defaultX;
    positionRef.current.width = defaultWidth;
    window.addEventListener('mousemove', handleMouseMove);
    window.addEventListener('mouseup', handleMouseUp);
    setResizing(true);
  });

  return (
    <div
      role="none"
      onMouseDown={handleMouseDown}
      {...otherProps}
    >
      {resizing && (
        <div style={{
          position: 'fixed',
          top: 0,
          left: 0,
          bottom: 0,
          right: 0,
          zIndex: 9999,
          cursor: 'col-resize',
        }}
        />
      )}
      {children}
    </div>
  );
};
export default observer(DragResize);
