import React from 'react';
import emptyPic from '../../image/empty_sprint.svg';
import './index.less';

const EmptyBlock = ({ height, des = '当前暂无冲刺', pic = emptyPic }) => (
  <div style={{ height: `${height}px` }} className="c7n-team-performance-empty">
    <div className="c7n-emptyBlock">
      <div className="c7n-imgWrap">
        <img src={pic} alt="" className="c7n-img" />
      </div>
      <div className="c7n-des">
        {des || ''}
      </div>
    </div>
  </div>
);

export default EmptyBlock;
