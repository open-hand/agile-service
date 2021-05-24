import React, { useRef } from 'react';
import { useSize } from 'ahooks';

interface AutoSizeProps {
  children: (size: ReturnType<typeof useSize>) => React.ReactNode
}
const AutoSize: React.FC<AutoSizeProps> = ({ children }) => {
  const ref = useRef<HTMLDivElement>(null);
  const size = useSize(ref.current);
  return (
    <div ref={ref} style={{ height: '100%', overflow: 'hidden' }}>
      {children(size)}
    </div>
  );
};

export default AutoSize;
