import React from 'react';
import { IStatus } from '@/common/types';
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
