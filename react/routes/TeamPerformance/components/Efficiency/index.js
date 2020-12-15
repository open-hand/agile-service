import React, { useContext } from 'react';
import Store from '../../stores';
import PieChart from './PieChart';
import LineChart from './LineChart';
import './index.less';

const Efficiency = () => {
  const { prefixCls } = useContext(Store);
  return (
    <div className={`${prefixCls}-container-effciency`}>
      <PieChart />
      <LineChart />
    </div>
  );
};

export default Efficiency;
