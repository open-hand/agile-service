import React, { HTMLAttributes, useRef } from 'react';
import { useSize } from 'ahooks';

interface AutoSizeProps extends HTMLAttributes<HTMLDivElement> {
  children: (size: ReturnType<typeof useSize>) => React.ReactNode
}
const AutoSize: React.FC<AutoSizeProps> = ({ children, ...props }) => {
  const ref = useRef<HTMLDivElement>(null);
  const size = useSize(ref.current);
  return (
    <div ref={ref} {...props} style={{ height: '100%', overflow: 'hidden', ...props.style }}>
      {children(size)}
    </div>
  );
};

export default AutoSize;
