/* eslint-disable react/self-closing-comp,jsx-a11y/accessible-emoji */
import React from 'react';
import { observer } from 'mobx-react-lite';
import EmptyScrumboard from './emptyScrumboard.svg';

const NoneSprint = ({ doingSprintExist, filterItems, hasSetFilter }) => {
  let tipTitle = '没有活跃的Sprint';
  const { sprint } = filterItems;
  if ((doingSprintExist || sprint) && Object.keys(filterItems).length === 1) {
    tipTitle = '当前冲刺下未规划问题';
  } else if (hasSetFilter) {
    tipTitle = '当前筛选条件下无问题';
  }

  return (
    <>
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          marginTop: '80px',
        }}
      >
        <img style={{ width: 170 }} src={EmptyScrumboard} alt="emptyscrumboard" />
        <div
          style={{
            marginLeft: 40,
          }}
        >
          <p style={{ color: 'rgba(0,0,0,0.65)', fontSize: '13px' }}>{`${tipTitle}`}</p>
          <p style={{ fontSize: 16, lineHeight: '34px' }}>
            在工作列表的
            <span style={{ color: '#5365EA' }}>待办事项</span>
            {!doingSprintExist ? '中开启冲刺' : '规划问题到当前冲刺'}
          </p>
        </div>
      </div>
    </>
  );
};

export default observer(NoneSprint);
