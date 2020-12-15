import React, { useContext } from 'react';
import Store from '../../stores';
import BugRankList from './BugRankList';
import QualityRankList from './QualityRankList';
import BugBarChart from './BugBarChart';
import './index.less';

const Quality = () => {
  const { prefixCls } = useContext(Store);
  return (
    <div className={`${prefixCls}-container-quality`}>
      <div className="quality-list-container">
        <BugRankList />
        <QualityRankList />
      </div>
      <div className="quality-chart-container">
        <BugBarChart />
      </div>
    </div>
  );
};

export default Quality;
