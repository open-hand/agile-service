import React from 'react';
import { observer } from 'mobx-react-lite';
import dayjs from 'dayjs';
import { Tooltip } from 'choerodon-ui/pro';
import { Gantt } from '@/components/gantt/types';
import STATUS_COLOR from '@/constants/STATUS_COLOR';

interface GanttBarProps {
  type: string
  bar: Gantt.Bar
  width: number
  height: number
}
function format(h: number) {
  if (h >= 24) {
    const days = Math.floor(h / 24);
    const hours = h % 24;

    return `${days}天${hours > 0 ? `${hours}小时` : ''}`;
  }
  return `${h}小时`;
}
const GanttBar: React.FC<GanttBarProps> = ({
  type, bar, width, height,
}) => {
  const { task: issue } = bar;
  const statusType = issue.statusVO.type;
  const subTasks = issue.children ? issue.children.filter((i) => i.issueTypeVO.typeCode === 'sub_task') : [];
  const hasChildren = subTasks && subTasks.length > 0;
  const totalCount = subTasks?.length || 0;
  const completeCount = subTasks?.filter((item) => item.completed).length || 0;
  // @ts-ignore
  let [color1, color2] = STATUS_COLOR[statusType];
  const percent = totalCount ? completeCount / totalCount : 0;
  let diff = 0;
  if (issue.estimatedStartTime && issue.estimatedEndTime) {
    // 延期
    if (dayjs(issue.estimatedEndTime).isBefore(dayjs()) && !issue.completed) {
      color1 = '#FF5C6A';
      color2 = '#FFBAC0';
    }
    diff = dayjs(issue.estimatedEndTime).diff(issue.estimatedStartTime, 'hour');
  }
  return (
    <Tooltip title={(
      <div>
        {issue.summary}
        <div>
          状态：
          {issue.statusVO.name}
        </div>
        <div>
          持续时间：
          {format(diff)}
        </div>
        {type !== 'assignee' && hasChildren && (
        <div>
          当前进度：
          {`${Math.round(percent * 100 * 100) / 100}%`}
        </div>
        )}
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
