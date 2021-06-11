import React from 'react';
import { EmptyPage } from '@choerodon/components';
import emptyPic from '../../image/empty_sprint.svg';
import './index.less';

const EmptyBlock = ({ height, des = '当前暂无冲刺', pic = emptyPic }) => (
  <EmptyPage image={pic} description={des} />
);

export default EmptyBlock;
