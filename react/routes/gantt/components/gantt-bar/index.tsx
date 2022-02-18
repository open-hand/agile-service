import React, {
  useContext, useRef, useCallback, useMemo, useEffect,
} from 'react';
import {
  observer, useComputed, useObservable, useObserver,
} from 'mobx-react-lite';
import { observable, toJS } from 'mobx';
import dayjs from 'dayjs';
import { Tooltip } from 'choerodon-ui/pro';
import type { GanttProps, Gantt } from '@choerodon/gantt';
import { GanntMoveWrap } from '@choerodon/gantt';

import { TooltipProps } from 'choerodon-ui/pro/lib/tooltip/Tooltip';
import { useCreation } from 'ahooks';
import STATUS_COLOR from '@/constants/STATUS_COLOR';
import styles from './index.less';
import type { GanttIssue } from '../../types';
import { useGanttBodyContext } from '../../stores/bodyContext';
import Bar from './Bar';

interface GanttBarProps {
  type: string
  bar: Gantt.Bar<GanttIssue>
  width: number
  height: number
  dateKeyRange: [string, string]
  onClick: GanttProps<GanttIssue>['onBarClick']
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
  type, bar, width, height, onClick, dateKeyRange,
}) => {
  const { store, processType } = useGanttBodyContext();
  const { ganttRef } = store;
  const estimateRef = useRef<HTMLDivElement>(null);
  const {
    record: issue, loading, stepGesture, task, dateMaps, startDateKey,
  } = bar;

  const statusType = issue.statusVO.type;
  const subTasks = issue.children ? issue.children.filter((i) => i.issueTypeVO?.typeCode === 'sub_task') : [];
  const {
    showPercent, totalCount, completeCount, percent,
  }: any = useComputed(() => {
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

  // @ts-ignore
  const [color1, color2] = STATUS_COLOR[statusType];
  // const percent = totalCount ? completeCount / totalCount : 0;
  let diff = 0;
  const delayDiff = 0;
  if (issue.estimatedStartTime && issue.estimatedEndTime) {
    // 延期
    // if (dayjs(issue.estimatedEndTime).isBefore(dayjs()) && !issue.completed) {
    //   color1 = '#FF5C6A';
    //   color2 = '#FFBAC0';
    // }
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
  const estimatedStartTime = dateMaps.get('estimatedStartTime');
  const estimatedEndTime = dateMaps.get('estimatedEndTime');
  const actualStartTime = dateMaps.get('actualStartTime');
  const actualEndTime = dateMaps.get('actualEndTime');

  // const fillDateRange = useObservable({
  //   start: actualStartTime, end: actualEndTime || estimatedEndTime || actualStartTime, completedColor: color1, unCompletedColor: color2,
  // });
  // const dashDateRange = useObservable({
  //   start: estimatedStartTime, end: estimatedEndTime || estimatedStartTime, color: color1,
  // });
  const dashDateRange = useCreation(() => (estimatedStartTime ? {
    start: toJS(estimatedStartTime),
    end: toJS(estimatedEndTime || estimatedStartTime),
    color: color1,
  } : undefined), [estimatedEndTime?.width, estimatedStartTime?.width, estimatedEndTime?.value, estimatedStartTime?.value]);
  const fillDateRange = useCreation(() => (actualStartTime ? {
    start: toJS(actualStartTime),
    end: toJS(actualEndTime || dashDateRange?.end || actualStartTime),
    completedColor: color1,
    unCompletedColor: color2,
    color: color1,
  } : undefined), [dashDateRange?.end, actualEndTime?.width, actualEndTime?.value, actualStartTime?.width, actualStartTime?.value]);
  const deadline = useCreation(() => {
    if (actualEndTime?.value) {
      return toJS(actualEndTime);
    }
    return (estimatedStartTime && estimatedEndTime ? dayjs().set('hour', 12).set('minute', 0).set('second', 0)
      .format('YYYY-MM-DD HH:mm:ss') : undefined);
  }, [actualEndTime, estimatedEndTime, estimatedStartTime, actualEndTime?.value]);
  return (
    <Bar
      ganttRef={ganttRef}
      bar={bar}
      width={width}
      height={height}
      tooltipTitle={tooltipTitle}
      progressCount={showPercent ? { completed: completeCount, total: totalCount } : undefined}
      startDate={dateMaps.get(startDateKey)}
      deadline={deadline}
      fillDateRange={fillDateRange}
      dashDateRange={dashDateRange}
    />
  );
};
export default observer(GanttBar);
