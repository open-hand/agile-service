import React from 'react';
import { IPriority } from '@/common/types';
import styles from './index.less';

interface PriorityTagProps {
  data: IPriority
  style?: React.CSSProperties
}
const PriorityTag: React.FC<PriorityTagProps> = ({ data, style }) => {
  const color = data?.colour || '#FFFFFF';
  return (
    <div
      className={styles.priority_tag}
      style={{
        backgroundColor: `${color}1F`,
        color,
        ...style,
      }}
    >
      {data?.name || ''}
    </div>
  );
};

export default PriorityTag;
