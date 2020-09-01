import React from 'react';
import STATUS from '@/constants/STATUS';

function StatusTag({ data: { valueCode, name } }) {
  return (
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
}
export default StatusTag;
