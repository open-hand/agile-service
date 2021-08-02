import React from 'react';
import { observer } from 'mobx-react';
import { Tooltip } from 'choerodon-ui/pro';
import styles from './SprintStoryPointInfo.less';

interface Props {
  show?: boolean,
  data: {
    todoStoryPoint: number,
    doingStoryPoint: number,
    doneStoryPoint: number,
  }
}

const SprintStoryPointInfo: React.FC<Props> = ({
  show = false,
  data: {
    todoStoryPoint, doingStoryPoint, doneStoryPoint,
  },
}) => (
  <div
    style={{
      display: show ? 'flex' : 'none',
    }}
    className={styles.c7n_sprintStoryPointInfo}
  >
    <Tooltip title={`待处理故事点: ${todoStoryPoint || 0}`}>
      <div style={{ backgroundColor: '#FFB100' }}>{todoStoryPoint || 0}</div>
    </Tooltip>
    <Tooltip title={`处理中故事点: ${doingStoryPoint || 0}`}>
      <div style={{ backgroundColor: '#4D90FE' }}>{doingStoryPoint || 0}</div>
    </Tooltip>
    <Tooltip title={`已完成故事点: ${doneStoryPoint || 0}`}>
      <div style={{ backgroundColor: '#00BFA5' }}>{doneStoryPoint || 0}</div>
    </Tooltip>
  </div>
);

export default observer(SprintStoryPointInfo);
