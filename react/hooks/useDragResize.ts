import React, { useCallback, useRef, useState } from 'react';

export default function useDragResize(handleResize: ({ width }: { width: number }) => void, {
  initSize,
  minWidth: minWidthConfig,
  maxWidth: maxWidthConfig,
}: {
  initSize: {
    width: number
  },
  minWidth: number
  maxWidth: number
}): [
    (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void,
    boolean
  ] {
  const [resizing, setResizing] = useState(false);
  const positionRef = useRef({
    left: 0,
  });
  const initSizeRef = useRef(initSize);
  const handleMouseMove = useCallback((event: MouseEvent) => {
    const distance = event.clientX - positionRef.current.left;
    const width = initSizeRef.current.width + distance;
    const minWidth = Math.max(width, minWidthConfig);
    const maxWidth = Math.min(minWidth, maxWidthConfig);
    handleResize({ width: maxWidth });
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [handleResize]);
  const handleMouseUp = useCallback(() => {
    window.removeEventListener('mousemove', handleMouseMove);
    window.removeEventListener('mouseup', handleMouseUp);
    setResizing(false);
  }, [handleMouseMove]);
  const handleMouseDown = useCallback((event: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
    positionRef.current.left = event.clientX;
    initSizeRef.current = initSize;
    window.addEventListener('mousemove', handleMouseMove);
    window.addEventListener('mouseup', handleMouseUp);
    setResizing(true);
  }, [handleMouseMove, handleMouseUp, initSize]);
  return [handleMouseDown, resizing];
}
