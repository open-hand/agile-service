import React, {
  useMemo,
} from 'react';
import {
  observer, useComputed,
} from 'mobx-react-lite';
import dayjs from 'dayjs';
import type { GanttProps, Gantt, GanttRef } from '@choerodon/gantt';

import type { GanttIssue } from '../../types';
import BarBase, { wrapGanttBaseBarFindDate } from './Bar';

const Bar = wrapGanttBaseBarFindDate(BarBase, { type: 'issue' });
interface GanttBarProps {
  type: string
  bar: Gantt.Bar<GanttIssue>
  width: number
  height: number
  dateKeyRange: [string, string]
  onClick: GanttProps<GanttIssue>['onBarClick']
  ganttRef: React.RefObject<GanttRef>
  processType?: string
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
  type, bar, width, height, onClick, dateKeyRange, ganttRef, processType,
}) => {
  const { record: issue } = bar;

  const subTasks = issue.children ? issue.children.filter((i) => i.issueTypeVO?.typeCode === 'sub_task') : [];
  const {
    showPercent, totalCount, completeCount, percent,
  }: any = useComputed(() => {
    if (!processType) {
      return {
        showPercent: false, totalCount: 0, completeCount: 0, percent: 0,
      };
    }
    const process = {
      showPercent: type !== 'assignee' && subTasks && subTasks.length > 0,
      totalCount: subTasks?.length || 0,
      completeCount: subTasks?.filter((item) => item.completed).length || 0,
      percent: 0,
    } as any;
    if (processType === 'task') {
      process.percent = process.totalCount ? process.completeCount / process.totalCount : 0;
      return process;
    }
    process.showPercent = true;
    process.percent = issue.workTimePercentage;
    const fakeCompleteCount = Math.ceil(process.percent * 100);
    process.completeCount = fakeCompleteCount;
    process.totalCount = 100;
    return process as any;
  }, [type, processType, issue.workTimePercentage, subTasks]);

  // const percent = totalCount ? completeCount / totalCount : 0;
  let diff = 0;
  let delayDiff = 0;
  if (issue.estimatedStartTime && issue.estimatedEndTime) {
    // 延期
    // if (dayjs(issue.estimatedEndTime).isBefore(dayjs()) && !issue.completed) {
    //   color1 = '#FF5C6A';
    //   color2 = '#FFBAC0';
    // }
    delayDiff = Math.max(0, dayjs(issue.actualEndTime || dayjs()).diff(issue.estimatedEndTime, 'hour'));
    diff = dayjs(issue.estimatedEndTime).diff(issue.estimatedStartTime, 'hour');
  }

  const tooltipTitle = useMemo(() => (
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
      {delayDiff > 0 && (
        <div>
          逾期：
          {format(delayDiff)}
        </div>
      )}
      {showPercent && (
        <div>
          当前进度：
          {`${Math.round(percent * 100 * 100) / 100}%`}
        </div>
      )}
      <div>
        预计开始：
        {issue.estimatedStartTime?.split(' ')[0] || '-'}
      </div>
      <div>
        预计结束：
        {issue.estimatedEndTime?.split(' ')[0] || '-'}
      </div>
      <div>
        实际开始：
        {issue.actualStartTime?.split(' ')[0] || '-'}
      </div>
      <div>
        实际结束：
        {issue.actualEndTime?.split(' ')[0] || '-'}
      </div>
    </div>
  ), [diff, issue.actualEndTime, issue.actualStartTime, issue.estimatedEndTime, issue.estimatedStartTime, issue.statusVO.name, issue.summary, percent, showPercent]);

  return (
    <Bar
      ganttRef={ganttRef}
      bar={bar}
      width={width}
      height={height}
      tooltipTitle={tooltipTitle}
      progressCount={showPercent ? { completed: completeCount, total: totalCount } : undefined}
    />
  );
};
GanttBar.defaultProps = {
  processType: undefined,
};
export default observer(GanttBar);
