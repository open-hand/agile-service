import React from 'react';
import STATUS from '@/constants/STATUS';
import { IStatus } from '@/common/types';
import styles from './index.less';

const getColorByCategoryCode = (categoryCode: IStatus['valueCode']) => {
  switch (categoryCode) {
    case 'todo':
      return 'rgb(255, 177, 0)';
    case 'doing':
      return 'rgb(77, 144, 254)';
    case 'done':
      return 'rgb(0, 191, 165)';
    case 'prepare':
      return '#F67F5A';
    default:
      return 'gray';
  }
};
interface StatusTagProps {
  name?: string,
  color?: string,
  data: IStatus
  style?: React.CSSProperties
  categoryCode?: IStatus['valueCode'],
}
const StatusTag: React.FC<StatusTagProps> = ({
  name,
  color,
  data,
  style,
  categoryCode,
}) => (
  <div
    className={styles.status_tag}
    style={{
      background: color || (categoryCode && getColorByCategoryCode(categoryCode)) || (data && STATUS[data.type]) || 'transparent',
      ...style,
    }}
  >
    {name || (data && data.name) || ''}
  </div>
);
export default StatusTag;
