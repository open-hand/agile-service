import React, { useContext } from 'react';
import Store from '../../stores';
import CompleteTendencyChart from './CompleteTendencyChart';
import BugTendencyChart from './BugTendencyChart';
import './index.less';

const Quality = () => {
  const { prefixCls } = useContext(Store);
  return (
    <div className={`${prefixCls}-container-tendency`}>
      <CompleteTendencyChart />
      <BugTendencyChart />
    </div>
  );
};

export default Quality;
