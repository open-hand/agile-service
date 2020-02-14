import React from 'react';
import { observer } from 'mobx-react';
import { Tooltip } from 'choerodon-ui';
import './StoryPoint.less';

const prefix = 'c7n-backlog-StoryPoint';
function StoryPoint({
  data: { 
    statusCode, todoStoryPoint, doingStoryPoint, doneStoryPoint, 
  },
}) {
  return (
    <div
      style={{
        display: statusCode === 'started' ? 'flex' : 'none',
      }}
      className={prefix}
    >
      <Tooltip title={`待处理故事点: ${todoStoryPoint}`}>
        <div style={{ backgroundColor: '#FFB100' }}>{todoStoryPoint || 0}</div>
      </Tooltip>
      <Tooltip title={`处理中故事点: ${doingStoryPoint}`}>
        <div style={{ backgroundColor: '#4D90FE' }}>{doingStoryPoint || 0}</div>
      </Tooltip>
      <Tooltip title={`已完成故事点: ${doneStoryPoint}`}>
        <div style={{ backgroundColor: '#00BFA5' }}>{doneStoryPoint || 0}</div>
      </Tooltip>
    </div>
  ); 
}

export default observer(StoryPoint);
