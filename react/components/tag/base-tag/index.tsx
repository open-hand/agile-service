import React from 'react';
import styles from './index.less';

interface BaseTagProps {
  color: string
  text: React.ReactNode
}

const BaseTag: React.FC<BaseTagProps> = ({ color, text }) => (
  <div
    className={styles.base_tag}
    style={{
      background: color,
    }}
  >
    {text || ''}
  </div>
);
export default BaseTag;
