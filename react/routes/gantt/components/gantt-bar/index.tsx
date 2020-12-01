import React from 'react';
import { observer } from 'mobx-react-lite';
import dayjs from 'dayjs';
import { Tooltip } from 'choerodon-ui/pro';
import { Gantt } from '@/components/gantt/types';
import STATUS_COLOR from '@/constants/STATUS_COLOR';

interface GanttBarProps {
  bar: Gantt.Bar
  width: number
  height: number
}
const GanttBar: React.FC<GanttBarProps> = ({ bar, width, height }) => {
  const { task: issue } = bar;
  const statusType = issue.statusVO.type;
  const totalCount = issue.children?.length || 0;
  // @ts-ignore
  const completeCount = issue.children?.filter((item) => item.statusVO.type === 'done').length || 0;
  // @ts-ignore
  const [color1, color2] = STATUS_COLOR[statusType];
  const percent = totalCount ? completeCount / totalCount : 0;
  let days = 0;
  if (issue.estimatedStartTime && issue.estimatedEndTime) {
    days = dayjs(issue.estimatedEndTime).diff(issue.estimatedStartTime, 'day');
  }
  return (
    <Tooltip title={(
      <div>
        {issue.summary}
        <div>
          持续时间：
          {days}
        </div>
        <div>
          当前进度：
          {`${Math.round(percent * 100 * 100) / 100}%`}
        </div>
        <div>
          预计开始：
          {issue.estimatedStartTime}
        </div>
        <div>
          预计结束：
          {issue.estimatedEndTime}
        </div>
      </div>
    )}
    >
      <div style={{
        width, height, backgroundColor: color2, borderColor: color1, display: 'flex', borderRadius: '3px', overflow: 'hidden',
      }}
      >
        <div style={{ flex: totalCount > 0 ? completeCount : 1, backgroundColor: color1 }} />
        <div style={{ flex: totalCount > 0 ? totalCount - completeCount : 0 }} />
      </div>
    </Tooltip>
  );
};
export default observer(GanttBar);
