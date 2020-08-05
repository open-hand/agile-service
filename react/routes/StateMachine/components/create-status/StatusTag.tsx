import React from 'react';
import STATUS from '@/constants/STATUS';
import { IStatus } from '@/common/types';

interface Props {
  data: IStatus
}
const StatusTag: React.FC<Props> = ({ data: { valueCode, name } }) => (
  <div style={{ display: 'inline-flex', justifyContent: 'flex-start', alignItems: 'center' }}>
    <div style={{
      width: 15,
      height: 15,
      borderRadius: 2,
      marginRight: 5,
      background: STATUS[valueCode] || 'rgb(255, 177, 0)',
    }}
    />
    {name}
  </div>
);
export default StatusTag;
